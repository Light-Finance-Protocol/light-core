(define-fungible-token points)

;; Define a map to track stacking balances
(define-map stackers
  { stx-address: principal }
  { balance: uint }
)

;; Define a map to track reward balances
(define-map rewards
  { stx-address: principal }
  { points: uint }
)

;; Function to stack STX
(define-public (stack (amount uint))
  (let (
        (stx-address (tx-sender))
      )
    ;; Assuming a mechanism to lock the STX tokens would be implemented here
    (map-set stackers
      { stx-address: stx-address }
      { balance: amount }
    )
    (ok true)
  )
)

;; Function to calculate rewards based on rules
(define-private (calculate-rewards (amount uint))
    (cond
      [(< amount 500) (/ 1 u100)] ;; 0.01 points per hour, assuming integer division
      [(and (>= amount 500) (< amount 3000)) u5]
      [(and (>= amount 3000) (< amount 10000)) u40]
      [else u150]
    )
)

;; Function to claim rewards (to be called periodically based on some time logic)
(define-public (claim-rewards)
  (let (
        (stx-address (tx-sender))
        (balance (default-to u0 (get balance (map-get? stackers {stx-address: stx-address}))))
        (rewards (calculate-rewards balance))
      )
    ;; Mint and transfer points to stx-address
    (ft-mint? points rewards stx-address)

    ;; Reset or update the stacker's balance if necessary
    ;; (map-set stackers {stx-address: stx-address} {balance: new-balance})

    (ok rewards)
  )
)