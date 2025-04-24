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

(define-constant minimum-collateral-ratio u150)
(define-constant liquidation-ratio u130)
(define-constant liquidation-penalty u10)
(define-constant minimum-collateral-amount u100000000)
(define-constant stablecoin-precision u1000000)

(define-data-var price-in-cents uint u100)
(define-data-var oracle-address principal contract-owner)
(define-data-var total-supply uint u0)
(define-data-var stability-fee uint u5)
(define-data-var last-fee-collection uint u0)

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
    (asserts! (or (is-eq current-debt u0) (>= (collateral-ratio new-collateral current-debt) minimum-collateral-ratio)) err-below-minimum-collateral)
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
    (asserts! (>= (collateral-ratio current-collateral new-debt) minimum-collateral-ratio) err-below-minimum-collateral)
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
    (asserts! (< ratio liquidation-ratio) err-liquidation-failed)
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