(define-non-fungible-token warehouse-receipt uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-receipt-exists (err u102))
(define-constant err-receipt-not-found (err u103))
(define-constant err-facility-not-verified (err u104))
(define-constant err-expired-receipt (err u105))

(define-data-var receipt-id-nonce uint u0)
(define-data-var total-receipts uint u0)

(define-map receipts uint 
  {
    owner: principal,
    facility: principal,
    commodity: (string-ascii 24),
    quantity: uint,
    issue-date: uint,
    expiry-date: uint,
    status: (string-ascii 10)
  }
)

(define-map verified-facilities principal bool)

(define-public (get-last-token-id)
    (ok (var-get receipt-id-nonce)))

(define-public (get-token-uri (token-id uint))
    (ok none))

(define-public (register-facility (facility-address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map-set verified-facilities facility-address true))))

(define-public (remove-facility (facility-address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map-set verified-facilities facility-address false))))

(define-read-only (is-facility-verified (facility principal))
  (default-to false (map-get? verified-facilities facility)))

(define-public (issue-receipt 
    (facility principal)
    (commodity (string-ascii 24))
    (quantity uint)
    (expiry-date uint))
  (let
    ((receipt-id (var-get receipt-id-nonce))
     (current-time stacks-block-height))
    (asserts! (is-facility-verified facility) err-facility-not-verified)
    (asserts! (> quantity u0) err-invalid-amount)
    (asserts! (> expiry-date current-time) err-invalid-amount)
    (try! (nft-mint? warehouse-receipt receipt-id tx-sender))
    (map-set receipts receipt-id
      {
        owner: tx-sender,
        facility: facility,
        commodity: commodity,
        quantity: quantity,
        issue-date: current-time,
        expiry-date: expiry-date,
        status: "active"
      })
    (var-set receipt-id-nonce (+ receipt-id u1))
    (var-set total-receipts (+ (var-get total-receipts) u1))
    (ok receipt-id)))

(define-public (transfer (id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (try! (nft-transfer? warehouse-receipt id sender recipient))
        (let ((receipt (unwrap! (map-get? receipts id) err-receipt-not-found)))
            (ok (map-set receipts id
                (merge receipt { owner: recipient }))))))

(define-public (release-goods (receipt-id uint))
  (let 
    ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found))
     (current-time stacks-block-height))
    (asserts! (is-eq (get owner receipt) tx-sender) err-not-authorized)
    (asserts! (< current-time (get expiry-date receipt)) err-expired-receipt)
    (try! (nft-burn? warehouse-receipt receipt-id tx-sender))
    (map-set receipts receipt-id
      (merge receipt { status: "released" }))
    (var-set total-receipts (- (var-get total-receipts) u1))
    (ok true)))

(define-read-only (get-receipt (receipt-id uint))
  (map-get? receipts receipt-id))

(define-read-only (get-receipt-owner (receipt-id uint))
  (nft-get-owner? warehouse-receipt receipt-id))

(define-read-only (get-total-receipts)
  (var-get total-receipts))

(define-read-only (is-receipt-expired (receipt-id uint))
  (ok (let ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found)))
    (> stacks-block-height (get expiry-date receipt)))))
