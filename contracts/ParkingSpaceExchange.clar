;; Parking Space Exchange Contract
;; A peer-to-peer parking space rental platform with real-time availability and dynamic pricing

;; Define the parking space NFT
(define-non-fungible-token parking-space uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-space-not-available (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-space-not-found (err u104))
(define-constant err-insufficient-payment (err u105))
(define-constant err-transfer-failed (err u106))

;; Data Variables
(define-data-var next-space-id uint u1)
(define-data-var platform-fee uint u5) ;; 5% platform fee

;; Data Maps
(define-map parking-spaces
  uint ;; space-id
  {
    owner: principal,
    location: (string-ascii 100),
    price-per-hour: uint,
    is-available: bool,
    rating: uint,
    total-bookings: uint
  })

(define-map bookings
  uint ;; booking-id
  {
    space-id: uint,
    renter: principal,
    start-time: uint,
    end-time: uint,
    total-amount: uint,
    is-active: bool
  })

(define-map space-earnings principal uint)
(define-data-var next-booking-id uint u1)

;; Function 1: List a parking space for rent
(define-public (list-parking-space (location (string-ascii 100)) (price-per-hour uint))
  (let 
    (
      (space-id (var-get next-space-id))
    )
    (begin
      ;; Validate inputs
      (asserts! (> price-per-hour u0) err-invalid-amount)
      (asserts! (> (len location) u0) err-invalid-amount)
      
      ;; Mint NFT to represent the parking space
      (try! (nft-mint? parking-space space-id tx-sender))
      
      ;; Store parking space details
      (map-set parking-spaces space-id
        {
          owner: tx-sender,
          location: location,
          price-per-hour: price-per-hour,
          is-available: true,
          rating: u50, ;; Default rating (5.0 * 10)
          total-bookings: u0
        })
      
      ;; Increment space ID for next listing
      (var-set next-space-id (+ space-id u1))
      
      ;; Print event for indexing
      (print {
        event: "space-listed",
        space-id: space-id,
        owner: tx-sender,
        location: location,
        price-per-hour: price-per-hour
      })
      
      (ok space-id))))

;; Function 2: Book a parking space
(define-public (book-parking-space (space-id uint) (duration-hours uint))
  (let 
    (
      (space-data (unwrap! (map-get? parking-spaces space-id) err-space-not-found))
      (booking-id (var-get next-booking-id))
      (total-amount (* (get price-per-hour space-data) duration-hours))
      (platform-fee-amount (/ (* total-amount (var-get platform-fee)) u100))
      (owner-amount (- total-amount platform-fee-amount))
      (start-time stacks-block-height)
      (end-time (+ stacks-block-height (* duration-hours u6))) ;; Assuming 6 blocks per hour
      (space-owner (get owner space-data))
    )
    (begin
      ;; Validate booking conditions
      (asserts! (get is-available space-data) err-space-not-available)
      (asserts! (> duration-hours u0) err-invalid-amount)
      (asserts! (not (is-eq tx-sender space-owner)) err-not-authorized)
      
      ;; Transfer owner's share directly to space owner
      (unwrap! (stx-transfer? owner-amount tx-sender space-owner) err-transfer-failed)
      
      ;; Transfer platform fee to contract
      (unwrap! (stx-transfer? platform-fee-amount tx-sender (as-contract tx-sender)) err-transfer-failed)
      
      ;; Update space availability
      (map-set parking-spaces space-id
        (merge space-data {
          is-available: false,
          total-bookings: (+ (get total-bookings space-data) u1)
        }))
      
      ;; Create booking record
      (map-set bookings booking-id
        {
          space-id: space-id,
          renter: tx-sender,
          start-time: start-time,
          end-time: end-time,
          total-amount: total-amount,
          is-active: true
        })
      
      ;; Update owner earnings
      (map-set space-earnings space-owner
        (+ (default-to u0 (map-get? space-earnings space-owner)) owner-amount))
      
      ;; Increment booking ID
      (var-set next-booking-id (+ booking-id u1))
      
      ;; Print event for indexing
      (print {
        event: "space-booked",
        booking-id: booking-id,
        space-id: space-id,
        renter: tx-sender,
        duration-hours: duration-hours,
        total-amount: total-amount
      })
      
      (ok booking-id))))

;; Read-only functions for data retrieval
(define-read-only (get-parking-space (space-id uint))
  (map-get? parking-spaces space-id))

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings booking-id))

(define-read-only (get-owner-earnings (owner principal))
  (default-to u0 (map-get? space-earnings owner)))

(define-read-only (get-next-space-id)
  (var-get next-space-id))

(define-read-only (get-platform-fee)
  (var-get platform-fee))