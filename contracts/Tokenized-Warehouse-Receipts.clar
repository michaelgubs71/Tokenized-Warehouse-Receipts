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
        (asserts! (is-none (map-get? collateralized-receipts id)) err-receipt-collateralized)
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

(define-constant err-batch-limit-exceeded (err u106))
(define-constant err-batch-empty (err u107))
(define-constant max-batch-size u50)

(define-public (batch-issue-receipts 
    (batch-data (list 50 {
        facility: principal,
        commodity: (string-ascii 24),
        quantity: uint,
        expiry-date: uint
    })))
  (let ((batch-length (len batch-data)))
    (asserts! (> batch-length u0) err-batch-empty)
    (asserts! (<= batch-length max-batch-size) err-batch-limit-exceeded)
    (ok (map process-single-issue batch-data))))

(define-private (process-single-issue (receipt-data {
    facility: principal,
    commodity: (string-ascii 24),
    quantity: uint,
    expiry-date: uint
}))
  (let ((receipt-id (var-get receipt-id-nonce))
        (current-time stacks-block-height))
    (asserts! (is-facility-verified (get facility receipt-data)) err-facility-not-verified)
    (asserts! (> (get quantity receipt-data) u0) err-invalid-amount)
    (asserts! (> (get expiry-date receipt-data) current-time) err-invalid-amount)
    (try! (nft-mint? warehouse-receipt receipt-id tx-sender))
    (map-set receipts receipt-id
      {
        owner: tx-sender,
        facility: (get facility receipt-data),
        commodity: (get commodity receipt-data),
        quantity: (get quantity receipt-data),
        issue-date: current-time,
        expiry-date: (get expiry-date receipt-data),
        status: "active"
      })
    (var-set receipt-id-nonce (+ receipt-id u1))
    (var-set total-receipts (+ (var-get total-receipts) u1))
    (ok receipt-id)))

(define-public (batch-transfer-receipts 
    (transfers (list 50 {
        id: uint,
        recipient: principal
    })))
  (let ((batch-length (len transfers)))
    (asserts! (> batch-length u0) err-batch-empty)
    (asserts! (<= batch-length max-batch-size) err-batch-limit-exceeded)
    (ok (map process-single-transfer transfers))))

(define-private (process-single-transfer (transfer-data {
    id: uint,
    recipient: principal
}))
  (let ((receipt-id (get id transfer-data))
        (recipient (get recipient transfer-data)))
    (try! (nft-transfer? warehouse-receipt receipt-id tx-sender recipient))
    (let ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found)))
      (map-set receipts receipt-id
        (merge receipt { owner: recipient }))
      (ok receipt-id))))

(define-public (batch-release-goods (receipt-ids (list 50 uint)))
  (let ((batch-length (len receipt-ids))
        (current-time stacks-block-height))
    (asserts! (> batch-length u0) err-batch-empty)
    (asserts! (<= batch-length max-batch-size) err-batch-limit-exceeded)
    (ok (map process-single-release receipt-ids))))

(define-private (process-single-release (receipt-id uint))
  (let ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found))
        (current-time stacks-block-height))
    (asserts! (is-eq (get owner receipt) tx-sender) err-not-authorized)
    (asserts! (< current-time (get expiry-date receipt)) err-expired-receipt)
    (try! (nft-burn? warehouse-receipt receipt-id tx-sender))
    (map-set receipts receipt-id
      (merge receipt { status: "released" }))
    (var-set total-receipts (- (var-get total-receipts) u1))
    (ok receipt-id)))

    (define-constant err-inspector-not-authorized (err u108))
(define-constant err-already-graded (err u109))
(define-constant err-invalid-grade (err u110))

(define-map authorized-inspectors principal bool)
(define-map receipt-grades uint {
    grade: (string-ascii 10),
    inspector: principal,
    inspection-date: uint,
    moisture-content: uint,
    purity-percentage: uint,
    notes: (string-ascii 100)
})

(define-map inspector-profiles principal {
    name: (string-ascii 50),
    certification: (string-ascii 30),
    active: bool
})

(define-data-var total-inspectors uint u0)

(define-public (authorize-inspector 
    (inspector principal)
    (name (string-ascii 50))
    (certification (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set authorized-inspectors inspector true)
    (map-set inspector-profiles inspector {
        name: name,
        certification: certification,
        active: true
    })
    (var-set total-inspectors (+ (var-get total-inspectors) u1))
    (ok true)))

(define-public (revoke-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set authorized-inspectors inspector false)
    (let ((profile (unwrap! (map-get? inspector-profiles inspector) err-not-authorized)))
      (map-set inspector-profiles inspector
        (merge profile { active: false })))
    (var-set total-inspectors (- (var-get total-inspectors) u1))
    (ok true)))

(define-read-only (is-inspector-authorized (inspector principal))
  (default-to false (map-get? authorized-inspectors inspector)))

(define-public (grade-commodity 
    (receipt-id uint)
    (grade (string-ascii 10))
    (moisture-content uint)
    (purity-percentage uint)
    (notes (string-ascii 100)))
  (let ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found))
        (current-time stacks-block-height))
    (asserts! (is-inspector-authorized tx-sender) err-inspector-not-authorized)
    (asserts! (is-none (map-get? receipt-grades receipt-id)) err-already-graded)
    (asserts! (<= purity-percentage u100) err-invalid-grade)
    (asserts! (is-eq (get status receipt) "active") err-not-authorized)
    (map-set receipt-grades receipt-id {
        grade: grade,
        inspector: tx-sender,
        inspection-date: current-time,
        moisture-content: moisture-content,
        purity-percentage: purity-percentage,
        notes: notes
    })
    (ok true)))

(define-public (update-grade 
    (receipt-id uint)
    (new-grade (string-ascii 10))
    (moisture-content uint)
    (purity-percentage uint)
    (notes (string-ascii 100)))
  (let ((existing-grade (unwrap! (map-get? receipt-grades receipt-id) err-receipt-not-found))
        (receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found))
        (current-time stacks-block-height))
    (asserts! (is-inspector-authorized tx-sender) err-inspector-not-authorized)
    (asserts! (is-eq (get inspector existing-grade) tx-sender) err-not-authorized)
    (asserts! (<= purity-percentage u100) err-invalid-grade)
    (asserts! (is-eq (get status receipt) "active") err-not-authorized)
    (map-set receipt-grades receipt-id {
        grade: new-grade,
        inspector: tx-sender,
        inspection-date: current-time,
        moisture-content: moisture-content,
        purity-percentage: purity-percentage,
        notes: notes
    })
    (ok true)))

(define-read-only (get-receipt-grade (receipt-id uint))
  (map-get? receipt-grades receipt-id))

(define-read-only (get-inspector-profile (inspector principal))
  (map-get? inspector-profiles inspector))

(define-read-only (get-total-inspectors)
  (var-get total-inspectors))

(define-read-only (get-receipts-by-grade (target-grade (string-ascii 10)))
  (ok target-grade))

(define-read-only (is-receipt-graded (receipt-id uint))
  (is-some (map-get? receipt-grades receipt-id)))

(define-constant err-price-oracle-not-authorized (err u111))
(define-constant err-invalid-price (err u112))
(define-constant err-commodity-not-supported (err u113))
(define-constant err-stale-price (err u114))

(define-map authorized-price-oracles principal bool)
(define-map commodity-prices (string-ascii 24) {
    price-per-unit: uint,
    last-updated: uint,
    oracle: principal,
    price-source: (string-ascii 50)
})

(define-map receipt-valuations uint {
    estimated-value: uint,
    valuation-date: uint,
    price-per-unit: uint
})

(define-data-var price-staleness-threshold uint u144)

(define-public (authorize-price-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map-set authorized-price-oracles oracle true))))

(define-public (revoke-price-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map-set authorized-price-oracles oracle false))))

(define-read-only (is-price-oracle-authorized (oracle principal))
  (default-to false (map-get? authorized-price-oracles oracle)))

(define-public (update-commodity-price 
    (commodity (string-ascii 24))
    (price-per-unit uint)
    (price-source (string-ascii 50)))
  (let ((current-time stacks-block-height))
    (asserts! (is-price-oracle-authorized tx-sender) err-price-oracle-not-authorized)
    (asserts! (> price-per-unit u0) err-invalid-price)
    (map-set commodity-prices commodity {
        price-per-unit: price-per-unit,
        last-updated: current-time,
        oracle: tx-sender,
        price-source: price-source
    })
    (ok true)))

(define-public (calculate-receipt-value (receipt-id uint))
  (let ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found))
        (commodity (get commodity receipt))
        (quantity (get quantity receipt))
        (current-time stacks-block-height))
    (let ((price-data (unwrap! (map-get? commodity-prices commodity) err-commodity-not-supported)))
      (let ((price-age (- current-time (get last-updated price-data))))
        (asserts! (<= price-age (var-get price-staleness-threshold)) err-stale-price)
        (let ((price-per-unit (get price-per-unit price-data))
              (estimated-value (* quantity price-per-unit)))
          (map-set receipt-valuations receipt-id {
              estimated-value: estimated-value,
              valuation-date: current-time,
              price-per-unit: price-per-unit
          })
          (ok estimated-value))))))

(define-public (batch-update-prices 
    (price-updates (list 20 {
        commodity: (string-ascii 24),
        price-per-unit: uint,
        price-source: (string-ascii 50)
    })))
  (let ((batch-length (len price-updates)))
    (asserts! (> batch-length u0) err-batch-empty)
    (asserts! (<= batch-length u20) err-batch-limit-exceeded)
    (ok (map process-price-update price-updates))))

(define-private (process-price-update (price-data {
    commodity: (string-ascii 24),
    price-per-unit: uint,
    price-source: (string-ascii 50)
}))
  (let ((current-time stacks-block-height)
        (commodity (get commodity price-data))
        (price-per-unit (get price-per-unit price-data))
        (price-source (get price-source price-data)))
    (asserts! (is-price-oracle-authorized tx-sender) err-price-oracle-not-authorized)
    (asserts! (> price-per-unit u0) err-invalid-price)
    (map-set commodity-prices commodity {
        price-per-unit: price-per-unit,
        last-updated: current-time,
        oracle: tx-sender,
        price-source: price-source
    })
    (ok commodity)))

(define-public (set-price-staleness-threshold (blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (> blocks u0) err-invalid-amount)
    (ok (var-set price-staleness-threshold blocks))))

(define-read-only (get-commodity-price (commodity (string-ascii 24)))
  (map-get? commodity-prices commodity))

(define-read-only (get-receipt-valuation (receipt-id uint))
  (map-get? receipt-valuations receipt-id))

(define-read-only (is-price-fresh (commodity (string-ascii 24)))
  (let ((current-time stacks-block-height))
    (match (map-get? commodity-prices commodity)
      price-data (let ((price-age (- current-time (get last-updated price-data))))
                   (<= price-age (var-get price-staleness-threshold)))
      false)))

(define-read-only (get-portfolio-value (receipt-ids (list 50 uint)))
  (let ((valuations (map get-individual-receipt-value receipt-ids)))
    (fold calculate-total-value valuations u0)))

(define-private (get-individual-receipt-value (receipt-id uint))
  (match (map-get? receipt-valuations receipt-id)
    valuation (get estimated-value valuation)
    u0))

(define-private (calculate-total-value (value uint) (total uint))
  (+ total value))

(define-read-only (get-price-staleness-threshold)
  (var-get price-staleness-threshold))

(define-constant err-receipt-collateralized (err u115))
(define-constant err-loan-not-found (err u116))
(define-constant err-loan-already-repaid (err u117))
(define-constant err-loan-not-due (err u118))
(define-constant err-insufficient-payment (err u119))
(define-constant err-invalid-loan-terms (err u120))

(define-data-var loan-id-nonce uint u0)

(define-map collateralized-receipts uint uint)

(define-map loans uint {
    borrower: principal,
    lender: principal,
    collateral-receipt-id: uint,
    principal-amount: uint,
    interest-rate: uint,
    loan-term: uint,
    loan-start: uint,
    repayment-amount: uint,
    status: (string-ascii 12)
})

(define-public (collateralize-receipt 
    (receipt-id uint)
    (lender principal)
    (principal-amount uint)
    (interest-rate uint)
    (loan-term uint))
  (let ((receipt (unwrap! (map-get? receipts receipt-id) err-receipt-not-found))
        (current-time stacks-block-height)
        (loan-id (var-get loan-id-nonce)))
    (asserts! (is-eq (get owner receipt) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status receipt) "active") err-not-authorized)
    (asserts! (is-none (map-get? collateralized-receipts receipt-id)) err-receipt-collateralized)
    (asserts! (> principal-amount u0) err-invalid-loan-terms)
    (asserts! (> loan-term u0) err-invalid-loan-terms)
    (asserts! (<= interest-rate u10000) err-invalid-loan-terms)
    (let ((repayment-amount (+ principal-amount (/ (* principal-amount interest-rate) u10000))))
      (map-set collateralized-receipts receipt-id loan-id)
      (map-set loans loan-id {
        borrower: tx-sender,
        lender: lender,
        collateral-receipt-id: receipt-id,
        principal-amount: principal-amount,
        interest-rate: interest-rate,
        loan-term: loan-term,
        loan-start: current-time,
        repayment-amount: repayment-amount,
        status: "active"
      })
      (var-set loan-id-nonce (+ loan-id u1))
      (ok loan-id))))

(define-public (repay-loan (loan-id uint) (payment-amount uint))
  (let ((loan (unwrap! (map-get? loans loan-id) err-loan-not-found))
        (current-time stacks-block-height))
    (asserts! (is-eq (get borrower loan) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status loan) "active") err-loan-already-repaid)
    (asserts! (>= payment-amount (get repayment-amount loan)) err-insufficient-payment)
    (let ((collateral-receipt-id (get collateral-receipt-id loan)))
      (map-delete collateralized-receipts collateral-receipt-id)
      (map-set loans loan-id
        (merge loan { status: "repaid" }))
      (ok true))))

(define-public (claim-collateral (loan-id uint))
  (let ((loan (unwrap! (map-get? loans loan-id) err-loan-not-found))
        (current-time stacks-block-height))
    (asserts! (is-eq (get lender loan) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status loan) "active") err-loan-already-repaid)
    (let ((loan-end (+ (get loan-start loan) (get loan-term loan)))
          (collateral-receipt-id (get collateral-receipt-id loan))
          (receipt (unwrap! (map-get? receipts collateral-receipt-id) err-receipt-not-found)))
      (asserts! (> current-time loan-end) err-loan-not-due)
      (try! (nft-transfer? warehouse-receipt collateral-receipt-id (get borrower loan) tx-sender))
      (map-delete collateralized-receipts collateral-receipt-id)
      (map-set receipts collateral-receipt-id
        (merge receipt { owner: tx-sender }))
      (map-set loans loan-id
        (merge loan { status: "defaulted" }))
      (ok true))))

(define-read-only (is-receipt-collateralized (receipt-id uint))
  (is-some (map-get? collateralized-receipts receipt-id)))

(define-read-only (get-loan-by-id (loan-id uint))
  (map-get? loans loan-id))

(define-read-only (get-loan-by-receipt (receipt-id uint))
  (match (map-get? collateralized-receipts receipt-id)
    loan-id (map-get? loans loan-id)
    none))

(define-read-only (is-loan-overdue (loan-id uint))
  (match (map-get? loans loan-id)
    loan (let ((current-time stacks-block-height)
               (loan-end (+ (get loan-start loan) (get loan-term loan))))
           (and (is-eq (get status loan) "active")
                (> current-time loan-end)))
    false))

(define-read-only (calculate-loan-value (loan-id uint))
  (match (map-get? loans loan-id)
    loan (let ((collateral-receipt-id (get collateral-receipt-id loan)))
           (match (map-get? receipt-valuations collateral-receipt-id)
             valuation (get estimated-value valuation)
             u0))
    u0))
