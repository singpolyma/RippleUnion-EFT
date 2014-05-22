CREATE TABLE accounts (
	id INTEGER PRIMARY KEY NOT NULL,
	vogogo_uuid TEXT NOT NULL
);

CREATE TABLE transactions (
	txhash TEXT PRIMARY KEY NOT NULL,
	ledger_index INTEGER NOT NULL,
	amount FLOAT NOT NULL,
	currency TEXT NOT NULL,
	dt INTEGER,
	invoiceid TEXT,
	paid_out FLOAT NOT NULL DEFAULT 0
);

CREATE TABLE verifications (
	item_id INTEGER NOT NULL,
	item_table TEXT NOT NULL,
	verification_type TEXT NOT NULL,
	notes TEXT,
	addr_token TEXT
);
