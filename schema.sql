CREATE TABLE accounts (
	id INTEGER PRIMARY KEY NOT NULL,
	vogogo_uuid TEXT NOT NULL
);

CREATE TABLE transactions (
	txhash TEXT PRIMARY KEY NOT NULL,
	ledger_index INTEGER NOT NULL,
	amount DECIMAL NOT NULL,
	currency TEXT NOT NULL,
	dt INTEGER,
	invoiceid TEXT,
	processed INTEGER NOT NULL DEFAULT 0
);
