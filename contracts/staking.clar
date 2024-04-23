(define-constant ERR-NOT-AUTHORIZED (err u1000))

(define-constant SECONDS_PER_HOUR u3600)
(define-constant REWARD_RATE_PERCENT_PER_HOUR_1 u1) ;; 0.01 point for reward 
(define-constant REWARD_RATE_PERCENT_PER_HOUR_5 u5) ;; 5 points reward 
(define-constant REWARD_RATE_PERCENT_PER_HOUR_40 u40) ;; 40 points reward
(define-constant REWARD_RATE_PERCENT_PER_HOUR_150 u150) ;; 150 points reward
(define-constant REWARD_RATE_DENOMINATOR u1000) ;; 1000 for a 0.1

(define-data-var contract-owner principal tx-sender)
(define-map approved-contracts principal bool)

(define-map deposit-balance { staker: principal } { amount: uint })
(define-map points-per-hour { staker: principal } { point: uint })

(define-map calculated-rewards { staker: principal } { points: uint })

(define-map stakes
    { staker: principal }
    { amount-staked: uint, last-reward-time: uint }
)

;; Error codes
(define-constant ERR_NOT_ENOUGH_BALANCE u1)
(define-constant ERR_WITHDRAWAL_FAILURE u2)

(define-data-var token-decimals uint u8)

(define-public (stake (amount uint))
    (begin
        (try! (calculate-reward tx-sender))
        (map-set stakes
            { staker: tx-sender }
            { amount-staked: (+ (default-to u0 (get amount-staked (map-get? stakes { staker: tx-sender }))) amount),
              last-reward-time: block-height }
        )
        (try! (set-point-per-hour tx-sender))
        (ok true)
    )
)

(define-private (set-point-per-hour (staker principal))
    (let ((stake-info (map-get? stakes { staker: tx-sender })))
        (if (is-some stake-info)
            (let ((current-amount (get amount-staked (unwrap! stake-info (err ERR_WITHDRAWAL_FAILURE)))))
                (if (< current-amount 5)
                    (begin
                        (map-set points-per-hour
                            { staker: staker }
                            { point: REWARD_RATE_PERCENT_PER_HOUR_1 }
                        )
                    )
                )
                (if (and (>= current-amount u5) (< current-amount u10))
                    (begin
                        (map-set points-per-hour
                            { staker: staker }
                            { point: REWARD_RATE_PERCENT_PER_HOUR_5 }
                        )
                    )
                )
                (if (and (>= current-amount u10) (< current-amount u20))
                    (begin
                        (map-set points-per-hour
                            { staker: staker }
                            { point: REWARD_RATE_PERCENT_PER_HOUR_40 }
                        )
                    )
                )
                (if (>= current-amount u20)
                    (begin
                        (map-set points-per-hour
                            { staker: staker }
                            { point: REWARD_RATE_PERCENT_PER_HOUR_150 }
                        )
                    )
                )
            )
            (ok true)
        )
        (ok false)
    )
)

(define-public (unstake (amount uint))
    (let ((stake-info (map-get? stakes { staker: tx-sender })))
        (if (is-some stake-info)
            (let ((current-amount (get amount-staked (unwrap! stake-info (err ERR_WITHDRAWAL_FAILURE)))))
                (if (>= current-amount amount)
                    (begin
                        (try! (calculate-reward tx-sender))
                        ;; Adjust stake amount
                        (map-set stakes
                            { staker: tx-sender }
                            { amount-staked: (- current-amount amount),
                              last-reward-time: block-height }
                        )
                        (ok true)
                    )
                    (err ERR_NOT_ENOUGH_BALANCE)
                )
            )
            (err ERR_WITHDRAWAL_FAILURE)
        )
    )
)

(define-private (calculate-reward (staker principal))
    (let ((stake-info (map-get? stakes {staker: staker})))
        (if (is-some stake-info)
            (let ((stake-data (unwrap! stake-info (err false)))
                  (current-time block-height))
                (let ((time-difference (- current-time (get last-reward-time stake-data)))
                      (amount-staked (get amount-staked stake-data))
                      (reward-point-rate (map-get? points-per-hour {staker: staker}))
                      (point-rate (unwrap! reward-point-rate (err false)))
                      )
                    (let ((hours-passed (/ time-difference SECONDS_PER_HOUR))
                    
                          (reward (/
                                  (* amount-staked hours-passed (get point point-rate)))))
                         ;; Update stake with new reward and reset last-reward-time
                         ;; Make sure to handle the reward payout to the staker
                         (map-set calculated-rewards
                            { staker: staker }
                            { points: (+ (default-to u0 (get points (map-get? calculated-rewards { staker: tx-sender }))) reward) }
                         )
                         (ok true)
                    )
                )
            )
            (err false)
        )
    )
)

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner))
    (ok (var-set contract-owner owner))
  )
)

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)

(define-private (check-is-approved)
  (ok (asserts! (default-to false (map-get? approved-contracts tx-sender)) ERR-NOT-AUTHORIZED))
) 

;; ------------------ Token decimals -----------------

(define-read-only (get-decimals)
	(ok (var-get token-decimals))
)

(define-constant ONE_8 u100000000)

;; @desc pow-decimals
;; @returns uint
(define-private (pow-decimals)
  (pow u10 (unwrap-panic (get-decimals)))
)

;; @desc fixed-to-decimals
;; @params amount
;; @returns uint
(define-read-only (fixed-to-decimals (amount uint))
  (/ (* amount (pow-decimals)) ONE_8)
)

;; @desc decimals-to-fixed 
;; @params amount
;; @returns uint
(define-private (decimals-to-fixed (amount uint))
  (/ (* amount ONE_8) (pow-decimals))
)
