;; VaultHealthMonitor Contract
;; Proactive health monitoring and alert system for vault management
;; Helps prevent liquidations through early warning systems

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u600))
(define-constant ERR_NO_VAULT (err u601))
(define-constant ERR_INVALID_THRESHOLD (err u602))
(define-constant ERR_ALERT_EXISTS (err u603))

;; Health score thresholds (in basis points)
(define-constant HEALTH_EXCELLENT u9500) ;; 95%
(define-constant HEALTH_GOOD u8000)      ;; 80%
(define-constant HEALTH_WARNING u6000)   ;; 60%
(define-constant HEALTH_CRITICAL u4000)  ;; 40%

;; Alert levels
(define-constant ALERT_GREEN u1)
(define-constant ALERT_YELLOW u2)
(define-constant ALERT_ORANGE u3)
(define-constant ALERT_RED u4)

(define-data-var alert-counter uint u0)
(define-data-var monitoring-enabled bool true)

;; Vault health metrics tracking
(define-map vault-health-history
  { owner: principal, checkpoint: uint }
  {
    collateral-ratio: uint,
    health-score: uint,
    alert-level: uint,
    debt-to-value: uint,
    recorded-at: uint
  }
)

;; Current vault health status
(define-map current-vault-health
  { owner: principal }
  {
    health-score: uint,
    alert-level: uint,
    last-updated: uint,
    trend-direction: uint, ;; 1: improving, 2: stable, 3: declining
    checkpoints-count: uint,
    last-alert-sent: uint
  }
)

;; Active alerts for vault owners
(define-map vault-alerts
  { alert-id: uint }
  {
    vault-owner: principal,
    alert-type: uint,
    severity: uint,
    message: (string-ascii 100),
    created-at: uint,
    acknowledged: bool,
    auto-generated: bool
  }
)

;; User alert preferences
(define-map alert-preferences
  { user: principal }
  {
    min-alert-level: uint,
    alert-frequency: uint, ;; blocks between alerts
    auto-optimize: bool,
    email-alerts: bool,
    emergency-contact: (optional principal)
  }
)

;; Optimization recommendations
(define-map health-recommendations
  { owner: principal }
  {
    recommendation-type: uint, ;; 1: add collateral, 2: repay debt, 3: rebalance
    suggested-amount: uint,
    urgency-level: uint,
    expected-improvement: uint,
    generated-at: uint
  }
)

;; Record vault health checkpoint
(define-public (record-health-checkpoint (vault-owner principal))
  (let (
    (vault-data (unwrap! (contract-call? .scc get-vault vault-owner) ERR_NO_VAULT))
    (collateral-ratio (contract-call? .scc get-collateral-ratio vault-owner))
    (health-score (calculate-health-score collateral-ratio))
    (alert-level (determine-alert-level health-score))
    (current-health (default-to 
      { health-score: u0, alert-level: u1, last-updated: u0, trend-direction: u2, checkpoints-count: u0, last-alert-sent: u0 }
      (map-get? current-vault-health { owner: vault-owner })))
    (checkpoint-id (get checkpoints-count current-health))
    (debt-to-value (calculate-debt-to-value vault-data))
  )
    (asserts! (var-get monitoring-enabled) ERR_UNAUTHORIZED)
    
    ;; Record historical checkpoint
    (map-set vault-health-history { owner: vault-owner, checkpoint: checkpoint-id } {
      collateral-ratio: collateral-ratio,
      health-score: health-score,
      alert-level: alert-level,
      debt-to-value: debt-to-value,
      recorded-at: stacks-block-height
    })
    
    ;; Update current health status
    (let (
      (trend (calculate-trend-direction (get health-score current-health) health-score))
    )
      (map-set current-vault-health { owner: vault-owner } {
        health-score: health-score,
        alert-level: alert-level,
        last-updated: stacks-block-height,
        trend-direction: trend,
        checkpoints-count: (+ checkpoint-id u1),
        last-alert-sent: (get last-alert-sent current-health)
      })
    )
    
    ;; Store health data and return success
    (ok {
      health-score: health-score,
      alert-level: alert-level,
      checkpoints-count: (+ checkpoint-id u1)
    })
  )
)

;; Create health alert
(define-public (create-health-alert (vault-owner principal) (severity uint) (auto-generated bool))
  (let (
    (alert-id (+ (var-get alert-counter) u1))
    (alert-message (generate-alert-message severity))
  )
    (map-set vault-alerts { alert-id: alert-id } {
      vault-owner: vault-owner,
      alert-type: u1, ;; Health alert type
      severity: severity,
      message: alert-message,
      created-at: stacks-block-height,
      acknowledged: false,
      auto-generated: auto-generated
    })
    
    (var-set alert-counter alert-id)
    
    ;; Update last alert sent time
    (match (map-get? current-vault-health { owner: vault-owner })
      current-health (map-set current-vault-health { owner: vault-owner }
        (merge current-health { last-alert-sent: stacks-block-height }))
      true
    )
    
    (ok alert-id)
  )
)

;; Generate optimization recommendation
(define-public (generate-optimization-recommendation (vault-owner principal))
  (let (
    (vault-data (unwrap! (contract-call? .scc get-vault vault-owner) ERR_NO_VAULT))
    (collateral-ratio (contract-call? .scc get-collateral-ratio vault-owner))
    (health-score (calculate-health-score collateral-ratio))
  )
    (let (
      (recommendation-type (determine-recommendation-type health-score collateral-ratio))
      (suggested-amount (calculate-suggested-amount vault-data recommendation-type))
      (urgency (determine-urgency-level health-score))
      (expected-improvement (calculate-expected-improvement suggested-amount recommendation-type))
    )
      (map-set health-recommendations { owner: vault-owner } {
        recommendation-type: recommendation-type,
        suggested-amount: suggested-amount,
        urgency-level: urgency,
        expected-improvement: expected-improvement,
        generated-at: stacks-block-height
      })
      
      (ok recommendation-type)
    )
  )
)

;; Acknowledge alert
(define-public (acknowledge-alert (alert-id uint))
  (let (
    (alert (unwrap! (map-get? vault-alerts { alert-id: alert-id }) ERR_NO_VAULT))
  )
    (asserts! (is-eq tx-sender (get vault-owner alert)) ERR_UNAUTHORIZED)
    (map-set vault-alerts { alert-id: alert-id }
      (merge alert { acknowledged: true })
    )
    (ok true)
  )
)

;; Set user alert preferences
(define-public (set-alert-preferences (min-level uint) (frequency uint) (auto-opt bool))
  (begin
    (asserts! (<= min-level u4) ERR_INVALID_THRESHOLD)
    (asserts! (>= frequency u144) ERR_INVALID_THRESHOLD) ;; At least 1 day between alerts
    
    (map-set alert-preferences { user: tx-sender } {
      min-alert-level: min-level,
      alert-frequency: frequency,
      auto-optimize: auto-opt,
      email-alerts: false,
      emergency-contact: none
    })
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-vault-health (owner principal))
  (map-get? current-vault-health { owner: owner })
)

(define-read-only (get-health-history (owner principal) (checkpoint uint))
  (map-get? vault-health-history { owner: owner, checkpoint: checkpoint })
)

(define-read-only (get-vault-alerts (vault-owner principal))
  ;; Returns list of recent unacknowledged alerts
  (filter-user-alerts vault-owner)
)

(define-read-only (get-alert-details (alert-id uint))
  (map-get? vault-alerts { alert-id: alert-id })
)

(define-read-only (get-optimization-recommendation (owner principal))
  (map-get? health-recommendations { owner: owner })
)

(define-read-only (get-user-alert-preferences (user principal))
  (map-get? alert-preferences { user: user })
)

(define-read-only (calculate-vault-health-score (vault-owner principal))
  ;; This function requires external contract call, so it should be used via record-health-checkpoint
  ;; Returns current health from stored data instead
  (match (map-get? current-vault-health { owner: vault-owner })
    health (ok (get health-score health))
    (err u0)
  )
)

;; Private helper functions

(define-private (calculate-health-score (collateral-ratio uint))
  ;; Health score based on distance from liquidation threshold (130%)
  (if (> collateral-ratio u20000)
    u10000 ;; Excellent health
    (if (> collateral-ratio u15000)
      u8000  ;; Good health
      (if (> collateral-ratio u14000)
        u6000 ;; Warning
        (if (> collateral-ratio u13000)
          u4000 ;; Critical
          u1000 ;; Emergency
        )
      )
    )
  )
)

(define-private (determine-alert-level (health-score uint))
  (if (>= health-score HEALTH_EXCELLENT) ALERT_GREEN
    (if (>= health-score HEALTH_GOOD) ALERT_YELLOW
      (if (>= health-score HEALTH_WARNING) ALERT_ORANGE
        ALERT_RED
      )
    )
  )
)

(define-private (calculate-debt-to-value (vault-data (tuple (collateral uint) (debt uint) (last-update uint))))
  ;; Calculate debt-to-value ratio as percentage
  (let (
    (collateral (get collateral vault-data))
    (debt (get debt vault-data))
  )
    (if (is-eq collateral u0)
      u0
      (/ (* debt u10000) collateral)
    )
  )
)

(define-private (calculate-trend-direction (previous-score uint) (current-score uint))
  (if (> current-score (+ previous-score u500))
    u1 ;; Improving
    (if (< current-score (- previous-score u500))
      u3 ;; Declining
      u2 ;; Stable
    )
  )
)

(define-private (should-send-alert (vault-owner principal) (alert-level uint))
  (let (
    (preferences (default-to 
      { min-alert-level: u2, alert-frequency: u1440, auto-optimize: false, email-alerts: false, emergency-contact: none }
      (map-get? alert-preferences { user: vault-owner })))
    (current-health (default-to
      { health-score: u0, alert-level: u1, last-updated: u0, trend-direction: u2, checkpoints-count: u0, last-alert-sent: u0 }
      (map-get? current-vault-health { owner: vault-owner })))
    (time-since-last-alert (- stacks-block-height (get last-alert-sent current-health)))
  )
    (and 
      (>= alert-level (get min-alert-level preferences))
      (>= time-since-last-alert (get alert-frequency preferences))
    )
  )
)

(define-private (generate-alert-message (severity uint))
  (if (is-eq severity ALERT_RED)
    "CRITICAL: Vault at risk of liquidation"
    (if (is-eq severity ALERT_ORANGE)
      "WARNING: Vault health declining"
      (if (is-eq severity ALERT_YELLOW)
        "CAUTION: Monitor vault closely"
        "INFO: Vault health good"
      )
    )
  )
)

(define-private (determine-recommendation-type (health-score uint) (collateral-ratio uint))
  (if (< health-score HEALTH_CRITICAL)
    u1 ;; Add collateral urgently
    (if (< collateral-ratio u16000)
      u2 ;; Repay debt
      u3 ;; Rebalance position
    )
  )
)

(define-private (calculate-suggested-amount (vault-data (tuple (collateral uint) (debt uint) (last-update uint))) (rec-type uint))
  ;; Simplified calculation - suggest 20% more collateral or 15% debt reduction
  (let (
    (collateral (get collateral vault-data))
    (debt (get debt vault-data))
  )
    (if (is-eq rec-type u1)
      (/ (* collateral u2000) u10000) ;; 20% more collateral
      (if (is-eq rec-type u2)
        (/ (* debt u1500) u10000) ;; 15% debt reduction
        (/ (* collateral u1000) u10000) ;; 10% rebalance
      )
    )
  )
)

(define-private (calculate-expected-improvement (suggested-amount uint) (rec-type uint))
  ;; Estimate health score improvement
  (if (is-eq rec-type u1)
    u2000 ;; Adding collateral improves health significantly
    (if (is-eq rec-type u2)
      u1500 ;; Debt reduction moderate improvement
      u1000 ;; Rebalancing minor improvement
    )
  )
)

(define-private (determine-urgency-level (health-score uint))
  (if (< health-score HEALTH_CRITICAL)
    u3 ;; High urgency
    (if (< health-score HEALTH_WARNING)
      u2 ;; Medium urgency
      u1 ;; Low urgency
    )
  )
)

(define-private (filter-user-alerts (vault-owner principal))
  ;; Simplified function to check if user has recent unacknowledged alerts
  ;; In a full implementation, this would return a filtered list
  (let (
    (current-health (map-get? current-vault-health { owner: vault-owner }))
  )
    (match current-health
      health (> (get alert-level health) ALERT_GREEN)
      false
    )
  )
)
