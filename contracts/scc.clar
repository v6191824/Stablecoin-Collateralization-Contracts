(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-enough-collateral (err u101))
(define-constant err-insufficient-stablecoin-balance (err u102))
(define-constant err-below-minimum-collateral (err u103))
(define-constant err-liquidation-failed (err u104))
(define-constant err-no-vault (err u105))
(define-constant err-vault-exists (err u106))
(define-constant err-price-error (err u107))
(define-constant err-unauthorized (err u108))
(define-constant err-invalid-amount (err u109))

(define-data-var minimum-collateral-ratio-var uint u150)
(define-data-var liquidation-ratio-var uint u130)
(define-data-var liquidation-penalty uint u10)
(define-data-var stability-fee-var uint u10)
(define-constant minimum-collateral-amount u100000000)
(define-constant stablecoin-precision u1000000)

(define-data-var price-in-cents uint u100)
(define-data-var oracle-address principal contract-owner)
(define-data-var total-supply uint u0)
(define-data-var stability-fee uint u5)
(define-data-var last-fee-collection uint u0)

(define-constant err-unsupported-collateral (err u200))
(define-constant err-collateral-exists (err u201))
(define-constant err-invalid-collateral-ratio (err u202))

(define-map supported-collaterals
  { asset-id: uint }
  {
    name: (string-ascii 32),
    min-collateral-ratio: uint,
    liquidation-ratio: uint,
    price-feed: principal,
    active: bool
  }
)

(define-map collateral-prices
  { asset-id: uint }
  { price: uint }
)

(define-map multi-vaults
  { owner: principal, asset-id: uint }
  {
    collateral: uint,
    debt: uint,
    last-update: uint
  }
)

(define-map user-collateral-assets
  { owner: principal }
  { asset-ids: (list 10 uint) }
)

(define-data-var next-asset-id uint u1)

(define-map vaults
  { owner: principal }
  {
    collateral: uint,
    debt: uint,
    last-update: uint
  }
)

(define-map stablecoin-balances
  { owner: principal }
  { balance: uint }
)

(define-fungible-token stablecoin)

(define-public (set-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle-address)) err-unauthorized)
    (asserts! (> new-price u0) err-invalid-amount)
    (ok (var-set price-in-cents new-price))
  )
)

(define-public (set-oracle (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set oracle-address new-oracle))
  )
)

(define-public (create-vault)
  (let ((sender tx-sender))
    (asserts! (is-none (map-get? vaults {owner: sender})) err-vault-exists)
    (ok (map-set vaults
      {owner: sender}
      {
        collateral: u0,
        debt: u0,
        last-update: stacks-block-height
      }
    ))
  )
)

(define-public (add-collateral (amount uint))
  (let (
    (sender tx-sender)
    (vault (unwrap! (map-get? vaults {owner: sender}) err-no-vault))
    (new-collateral (+ (get collateral vault) amount))
  )
    (asserts! (>= amount u0) err-invalid-amount)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (ok (map-set vaults
      {owner: sender}
      {
        collateral: new-collateral,
        debt: (get debt vault),
        last-update: stacks-block-height
      }
    ))
  )
)

(define-public (remove-collateral (amount uint))
  (let (
    (sender tx-sender)
    (vault (unwrap! (map-get? vaults {owner: sender}) err-no-vault))
    (current-collateral (get collateral vault))
    (current-debt (get debt vault))
    (new-collateral (- current-collateral amount))
  )
    (asserts! (>= amount u0) err-invalid-amount)
    (asserts! (<= amount current-collateral) err-not-enough-collateral)
    (asserts! (or (is-eq current-debt u0) (>= (collateral-ratio new-collateral current-debt) (var-get minimum-collateral-ratio-var))) err-below-minimum-collateral)
    (try! (as-contract (stx-transfer? amount tx-sender sender)))
    (ok (map-set vaults
      {owner: sender}
      {
        collateral: new-collateral,
        debt: current-debt,
        last-update: stacks-block-height
      }
    ))
  )
)

(define-public (mint-stablecoin (amount uint))
  (let (
    (sender tx-sender)
    (vault (unwrap! (map-get? vaults {owner: sender}) err-no-vault))
    (current-collateral (get collateral vault))
    (current-debt (get debt vault))
    (new-debt (+ current-debt amount))
    (user-balance (default-to {balance: u0} (map-get? stablecoin-balances {owner: sender})))
    (new-balance (+ (get balance user-balance) amount))
  )
    (asserts! (>= amount u0) err-invalid-amount)
    (asserts! (>= (collateral-ratio current-collateral new-debt) (var-get minimum-collateral-ratio-var)) err-below-minimum-collateral)
    (map-set vaults
      {owner: sender}
      {
        collateral: current-collateral,
        debt: new-debt,
        last-update: stacks-block-height
      }
    )
    (map-set stablecoin-balances
      {owner: sender}
      {balance: new-balance}
    )
    (var-set total-supply (+ (var-get total-supply) amount))
    (ft-mint? stablecoin amount sender)
  )
)

(define-public (burn-stablecoin (amount uint))
  (let (
    (sender tx-sender)
    (vault (unwrap! (map-get? vaults {owner: sender}) err-no-vault))
    (current-collateral (get collateral vault))
    (current-debt (get debt vault))
    (new-debt (- current-debt amount))
    (user-balance (default-to {balance: u0} (map-get? stablecoin-balances {owner: sender})))
    (new-balance (- (get balance user-balance) amount))
  )
    (asserts! (>= amount u0) err-invalid-amount)
    (asserts! (<= amount current-debt) err-invalid-amount)
    (asserts! (>= (get balance user-balance) amount) err-insufficient-stablecoin-balance)
    (try! (ft-burn? stablecoin amount sender))
    (map-set vaults
      {owner: sender}
      {
        collateral: current-collateral,
        debt: new-debt,
        last-update: stacks-block-height
      }
    )
    (map-set stablecoin-balances
      {owner: sender}
      {balance: new-balance}
    )
    (var-set total-supply (- (var-get total-supply) amount))
    (ok true)
  )
)

(define-public (liquidate (vault-owner principal))
  (let (
    (vault (unwrap! (map-get? vaults {owner: vault-owner}) err-no-vault))
    (collateral-amount (get collateral vault))
    (debt-amount (get debt vault))
    (ratio (collateral-ratio collateral-amount debt-amount))
  )
    (asserts! (< ratio (var-get liquidation-ratio-var)) err-liquidation-failed)
    (try! (ft-burn? stablecoin debt-amount tx-sender))
    (try! (as-contract (stx-transfer? collateral-amount tx-sender tx-sender)))
    (map-delete vaults {owner: vault-owner})
    (var-set total-supply (- (var-get total-supply) debt-amount))
    (ok true)
  )
)

(define-read-only (get-vault (owner principal))
  (map-get? vaults {owner: owner})
)

(define-read-only (get-stablecoin-balance (owner principal))
  (default-to {balance: u0} (map-get? stablecoin-balances {owner: owner}))
)

(define-read-only (get-price)
  (var-get price-in-cents)
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (collateral-ratio (collateral-amount uint) (debt-amount uint))
  (if (is-eq debt-amount u0)
    u0
    (/ (* (* collateral-amount (var-get price-in-cents)) u100) debt-amount)
  )
)

(define-read-only (get-collateral-ratio (owner principal))
  (let (
    (vault (default-to {collateral: u0, debt: u0, last-update: u0} (map-get? vaults {owner: owner})))
  )
    (collateral-ratio (get collateral vault) (get debt vault))
  )
)

(define-public (transfer-stablecoin (amount uint) (recipient principal))
  (let (
    (sender tx-sender)
    (sender-balance (default-to {balance: u0} (map-get? stablecoin-balances {owner: sender})))
    (recipient-balance (default-to {balance: u0} (map-get? stablecoin-balances {owner: recipient})))
  )
    (asserts! (>= (get balance sender-balance) amount) err-insufficient-stablecoin-balance)
    (map-set stablecoin-balances
      {owner: sender}
      {balance: (- (get balance sender-balance) amount)}
    )
    (map-set stablecoin-balances
      {owner: recipient}
      {balance: (+ (get balance recipient-balance) amount)}
    )
    (try! (ft-transfer? stablecoin amount sender recipient))
    (ok true)
  )
)


(define-constant timelock-period u144) ;; 24 hours in blocks
(define-constant err-pending-change (err u110))
(define-constant err-no-pending-change (err u111))
(define-constant err-timelock-active (err u112))

(define-map parameter-changes
    { parameter: (string-ascii 24) }
    {
        new-value: uint,
        activation-height: uint
    }
)

(define-public (queue-parameter-change (parameter (string-ascii 24)) (new-value uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? parameter-changes {parameter: parameter})) err-pending-change)
        (ok (map-set parameter-changes
            {parameter: parameter}
            {
                new-value: new-value,
                activation-height: (+ stacks-block-height timelock-period)
            }
        ))
    )
)


(define-constant err-flash-loan-failed (err u113))

(define-public (flash-loan (amount uint) (recipient principal))
    (let (
        (current-supply (var-get total-supply))
        (fee (/ (* amount u1) u1000))
    )
        (asserts! (> amount u0) err-invalid-amount)
        (try! (ft-mint? stablecoin amount recipient))
        (let ((repay-amount (+ amount fee)))
            (asserts! (>= (ft-get-balance stablecoin recipient) repay-amount) err-flash-loan-failed)
            (try! (ft-burn? stablecoin repay-amount recipient))
            (var-set total-supply (+ current-supply fee))
            (ok true)
        )
    )
)

(define-public (execute-parameter-change (parameter (string-ascii 24)))
    (let (
        (pending-change (unwrap! (map-get? parameter-changes {parameter: parameter}) err-no-pending-change))
        (activation-height (get activation-height pending-change))
    )
        (asserts! (< stacks-block-height activation-height) err-timelock-active)
        (map-set parameter-changes
            {parameter: parameter}
            {
                new-value: (get new-value pending-change),
                activation-height: u0
            }
        )
        (ok true)
    )
)



(define-public (add-collateral-type 
  (name (string-ascii 32))
  (min-ratio uint)
  (liq-ratio uint)
  (price-feed principal))
  (let ((asset-id (var-get next-asset-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> min-ratio u100) err-invalid-collateral-ratio)
    (asserts! (> liq-ratio u100) err-invalid-collateral-ratio)
    (asserts! (< liq-ratio min-ratio) err-invalid-collateral-ratio)
    (map-set supported-collaterals
      { asset-id: asset-id }
      {
        name: name,
        min-collateral-ratio: min-ratio,
        liquidation-ratio: liq-ratio,
        price-feed: price-feed,
        active: true
      }
    )
    (var-set next-asset-id (+ asset-id u1))
    (ok asset-id)
  )
)

(define-public (set-collateral-price (asset-id uint) (new-price uint))
  (let ((collateral-info (unwrap! (map-get? supported-collaterals { asset-id: asset-id }) err-unsupported-collateral)))
    (asserts! (is-eq tx-sender (get price-feed collateral-info)) err-unauthorized)
    (asserts! (> new-price u0) err-invalid-amount)
    (ok (map-set collateral-prices
      { asset-id: asset-id }
      { price: new-price }
    ))
  )
)

(define-public (create-multi-vault (asset-id uint))
  (let (
    (sender tx-sender)
    (collateral-info (unwrap! (map-get? supported-collaterals { asset-id: asset-id }) err-unsupported-collateral))
    (user-assets (default-to { asset-ids: (list) } (map-get? user-collateral-assets { owner: sender })))
  )
    (asserts! (get active collateral-info) err-unsupported-collateral)
    (asserts! (is-none (map-get? multi-vaults { owner: sender, asset-id: asset-id })) err-vault-exists)
    (map-set multi-vaults
      { owner: sender, asset-id: asset-id }
      {
        collateral: u0,
        debt: u0,
        last-update: stacks-block-height
      }
    )
    (map-set user-collateral-assets
      { owner: sender }
      { asset-ids: (unwrap! (as-max-len? (append (get asset-ids user-assets) asset-id) u10) err-invalid-amount) }
    )
    (ok true)
  )
)

(define-public (add-multi-collateral (asset-id uint) (amount uint))
  (let (
    (sender tx-sender)
    (vault (unwrap! (map-get? multi-vaults { owner: sender, asset-id: asset-id }) err-no-vault))
    (collateral-info (unwrap! (map-get? supported-collaterals { asset-id: asset-id }) err-unsupported-collateral))
    (new-collateral (+ (get collateral vault) amount))
  )
    (asserts! (get active collateral-info) err-unsupported-collateral)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (if (is-eq asset-id u0)
      (stx-transfer? amount sender (as-contract tx-sender))
      (ok true)
    ))
    (ok (map-set multi-vaults
      { owner: sender, asset-id: asset-id }
      {
        collateral: new-collateral,
        debt: (get debt vault),
        last-update: stacks-block-height
      }
    ))
  )
)
(define-public (mint-from-multi-collateral (asset-id uint) (amount uint))
  (let (
    (sender tx-sender)
    (vault (unwrap! (map-get? multi-vaults { owner: sender, asset-id: asset-id }) err-no-vault))
    (collateral-info (unwrap! (map-get? supported-collaterals { asset-id: asset-id }) err-unsupported-collateral))
    (current-collateral (get collateral vault))
    (current-debt (get debt vault))
    (new-debt (+ current-debt amount))
    (user-balance (default-to { balance: u0 } (map-get? stablecoin-balances { owner: sender })))
    (new-balance (+ (get balance user-balance) amount))
  )
    (asserts! (get active collateral-info) err-unsupported-collateral)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (multi-collateral-ratio asset-id current-collateral new-debt) (get min-collateral-ratio collateral-info)) err-below-minimum-collateral)
    (map-set multi-vaults
      { owner: sender, asset-id: asset-id }
      {
        collateral: current-collateral,
        debt: new-debt,
        last-update: stacks-block-height
      }
    )
    (map-set stablecoin-balances
      { owner: sender }
      { balance: new-balance }
    )
    (var-set total-supply (+ (var-get total-supply) amount))
    (ft-mint? stablecoin amount sender)
  )
)

(define-public (liquidate-multi-vault (vault-owner principal) (asset-id uint))
  (let (
    (vault (unwrap! (map-get? multi-vaults { owner: vault-owner, asset-id: asset-id }) err-no-vault))
    (collateral-info (unwrap! (map-get? supported-collaterals { asset-id: asset-id }) err-unsupported-collateral))
    (collateral-amount (get collateral vault))
    (debt-amount (get debt vault))
    (ratio (multi-collateral-ratio asset-id collateral-amount debt-amount))
  )
    (asserts! (< ratio (get liquidation-ratio collateral-info)) err-liquidation-failed)
    (try! (ft-burn? stablecoin debt-amount tx-sender))
    (try! (if (is-eq asset-id u0)
      (as-contract (stx-transfer? collateral-amount tx-sender tx-sender))
      (ok true)
    ))
    (map-delete multi-vaults { owner: vault-owner, asset-id: asset-id })
    (var-set total-supply (- (var-get total-supply) debt-amount))
    (ok true)
  )
)
(define-read-only (get-multi-vault (owner principal) (asset-id uint))
  (map-get? multi-vaults { owner: owner, asset-id: asset-id })
)

(define-read-only (get-collateral-info (asset-id uint))
  (map-get? supported-collaterals { asset-id: asset-id })
)

(define-read-only (get-collateral-price (asset-id uint))
  (default-to { price: u0 } (map-get? collateral-prices { asset-id: asset-id }))
)

(define-read-only (multi-collateral-ratio (asset-id uint) (collateral-amount uint) (debt-amount uint))
  (let ((price-info (get-collateral-price asset-id)))
    (if (is-eq debt-amount u0)
      u0
      (/ (* (* collateral-amount (get price price-info)) u100) debt-amount)
    )
  )
)

(define-read-only (get-user-collateral-assets (owner principal))
  (default-to { asset-ids: (list) } (map-get? user-collateral-assets { owner: owner }))
)

(define-read-only (get-total-collateral-value (owner principal))
  (let ((user-assets (get asset-ids (get-user-collateral-assets owner))))
    (fold calculate-asset-value user-assets u0)
  )
)

(define-private (calculate-asset-value (asset-id uint) (total-value uint))
  (let (
    (vault (default-to { collateral: u0, debt: u0, last-update: u0 } 
                      (map-get? multi-vaults { owner: tx-sender, asset-id: asset-id })))
    (price-info (get-collateral-price asset-id))
  )
    (+ total-value (* (get collateral vault) (get price price-info)))
  )
)