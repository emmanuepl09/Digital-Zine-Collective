;; Digital Zine Collective - Independent publication network with creator rewards
;; A decentralized platform for digital zine publishing and creator monetization

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-already-purchased (err u107))

;; Data Variables
(define-data-var next-zine-id uint u1)
(define-data-var next-creator-id uint u1)
(define-data-var platform-fee-rate uint u500) ;; 5% in basis points
(define-data-var total-platform-revenue uint u0)

;; Data Maps
(define-map zines uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    price: uint,
    content-hash: (string-ascii 64),
    created-at: uint,
    total-sales: uint,
    revenue: uint,
    is-active: bool
})

(define-map creators principal {
    name: (string-ascii 50),
    bio: (string-ascii 300),
    total-zines: uint,
    total-revenue: uint,
    reputation-score: uint,
    is-verified: bool,
    joined-at: uint
})

(define-map creator-followers {follower: principal, creator: principal} bool)
(define-map zine-purchases {buyer: principal, zine-id: uint} {
    purchased-at: uint,
    price-paid: uint
})

(define-map zine-ratings {rater: principal, zine-id: uint} {
    rating: uint,
    review: (string-ascii 200)
})

(define-map creator-payouts principal uint)

;; Helper Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner))

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-rate)) u10000))

(define-private (calculate-creator-payout (amount uint))
    (- amount (calculate-platform-fee amount)))

;; Public Functions

;; Register as a creator
(define-public (register-creator (name (string-ascii 50)) (bio (string-ascii 300)))
    (let ((creator-data {
            name: name,
            bio: bio,
            total-zines: u0,
            total-revenue: u0,
            reputation-score: u100,
            is-verified: false,
            joined-at: block-height
        }))
        (asserts! (is-none (map-get? creators tx-sender)) err-already-exists)
        (map-set creators tx-sender creator-data)
        (var-set next-creator-id (+ (var-get next-creator-id) u1))
        (ok true)))

;; Create a new zine
(define-public (create-zine 
    (title (string-ascii 100)) 
    (description (string-ascii 500))
    (price uint)
    (content-hash (string-ascii 64)))
    (let ((zine-id (var-get next-zine-id))
          (zine-data {
              title: title,
              description: description,
              creator: tx-sender,
              price: price,
              content-hash: content-hash,
              created-at: block-height,
              total-sales: u0,
              revenue: u0,
              is-active: true
          }))
        (asserts! (is-some (map-get? creators tx-sender)) err-unauthorized)
        (asserts! (> price u0) err-invalid-price)
        (map-set zines zine-id zine-data)
        (map-set creators tx-sender 
            (merge (unwrap-panic (map-get? creators tx-sender))
                   {total-zines: (+ (get total-zines (unwrap-panic (map-get? creators tx-sender))) u1)}))
        (var-set next-zine-id (+ zine-id u1))
        (ok zine-id)))

;; Purchase a zine
(define-public (purchase-zine (zine-id uint))
    (let ((zine (unwrap! (map-get? zines zine-id) err-not-found))
          (price (get price zine))
          (creator (get creator zine))
          (platform-fee (calculate-platform-fee price))
          (creator-payout (calculate-creator-payout price)))
        (asserts! (get is-active zine) err-not-found)
        (asserts! (is-none (map-get? zine-purchases {buyer: tx-sender, zine-id: zine-id})) err-already-purchased)
        
        ;; Transfer payment
        (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
        
        ;; Record purchase
        (map-set zine-purchases 
            {buyer: tx-sender, zine-id: zine-id}
            {purchased-at: block-height, price-paid: price})
        
        ;; Update zine stats
        (map-set zines zine-id 
            (merge zine {
                total-sales: (+ (get total-sales zine) u1),
                revenue: (+ (get revenue zine) price)
            }))
        
        ;; Update creator stats and add to payouts
        (let ((creator-data (unwrap-panic (map-get? creators creator))))
            (map-set creators creator 
                (merge creator-data {
                    total-revenue: (+ (get total-revenue creator-data) creator-payout)
                }))
            (map-set creator-payouts creator 
                (+ (default-to u0 (map-get? creator-payouts creator)) creator-payout)))
        
        ;; Update platform revenue
        (var-set total-platform-revenue (+ (var-get total-platform-revenue) platform-fee))
        (ok true)))

;; Follow a creator
(define-public (follow-creator (creator principal))
    (begin
        (asserts! (is-some (map-get? creators creator)) err-not-found)
        (asserts! (not (is-eq tx-sender creator)) err-unauthorized)
        (map-set creator-followers {follower: tx-sender, creator: creator} true)
        (ok true)))

;; Unfollow a creator
(define-public (unfollow-creator (creator principal))
    (begin
        (map-delete creator-followers {follower: tx-sender, creator: creator})
        (ok true)))

;; Rate a zine
(define-public (rate-zine (zine-id uint) (rating uint) (review (string-ascii 200)))
    (let ((zine (unwrap! (map-get? zines zine-id) err-not-found)))
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-amount)
        (asserts! (is-some (map-get? zine-purchases {buyer: tx-sender, zine-id: zine-id})) err-unauthorized)
        (map-set zine-ratings {rater: tx-sender, zine-id: zine-id} 
            {rating: rating, review: review})
        (ok true)))

;; Creator withdraws earnings
(define-public (withdraw-earnings)
    (let ((payout-amount (default-to u0 (map-get? creator-payouts tx-sender))))
        (asserts! (> payout-amount u0) err-insufficient-funds)
        (map-delete creator-payouts tx-sender)
        (try! (as-contract (stx-transfer? payout-amount tx-sender tx-sender)))
        (ok payout-amount)))

;; Update zine status (creator only)
(define-public (toggle-zine-status (zine-id uint))
    (let ((zine (unwrap! (map-get? zines zine-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator zine)) err-unauthorized)
        (map-set zines zine-id 
            (merge zine {is-active: (not (get is-active zine))}))
        (ok true)))

;; Admin function to verify creator
(define-public (verify-creator (creator-principal principal))
    (let ((creator-data (unwrap! (map-get? creators creator-principal) err-not-found)))
        (asserts! (is-contract-owner) err-owner-only)
        (map-set creators creator-principal 
            (merge creator-data {is-verified: true, reputation-score: u200}))
        (ok true)))

;; Admin function to update platform fee
(define-public (update-platform-fee (new-fee-rate uint))
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (<= new-fee-rate u2000) err-invalid-amount) ;; Max 20%
        (var-set platform-fee-rate new-fee-rate)
        (ok true)))

;; Admin function to withdraw platform revenue
(define-public (withdraw-platform-revenue)
    (let ((revenue (var-get total-platform-revenue)))
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (> revenue u0) err-insufficient-funds)
        (var-set total-platform-revenue u0)
        (try! (as-contract (stx-transfer? revenue tx-sender contract-owner)))
        (ok revenue)))

;; Read-only functions

(define-read-only (get-zine (zine-id uint))
    (map-get? zines zine-id))

(define-read-only (get-creator (creator-principal principal))
    (map-get? creators creator-principal))

(define-read-only (get-zine-purchase (buyer principal) (zine-id uint))
    (map-get? zine-purchases {buyer: buyer, zine-id: zine-id}))

(define-read-only (get-creator-payout (creator-principal principal))
    (default-to u0 (map-get? creator-payouts creator-principal)))

(define-read-only (is-following (follower principal) (creator principal))
    (default-to false (map-get? creator-followers {follower: follower, creator: creator})))

(define-read-only (get-zine-rating (rater principal) (zine-id uint))
    (map-get? zine-ratings {rater: rater, zine-id: zine-id}))

(define-read-only (get-platform-stats)
    {
        total-revenue: (var-get total-platform-revenue),
        fee-rate: (var-get platform-fee-rate),
        total-zines: (- (var-get next-zine-id) u1),
        total-creators: (- (var-get next-creator-id) u1)
    })

(define-read-only (has-purchased-zine (buyer principal) (zine-id uint))
    (is-some (map-get? zine-purchases {buyer: buyer, zine-id: zine-id})))

;; Initialize contract
(begin
    (print "Digital Zine Collective initialized")
    (print "Ready for creators and readers!"))