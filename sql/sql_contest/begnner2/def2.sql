CREATE TABLE IF NOT EXISTS ORDERS (
    ORDER_NO VARCHAR NOT NULL PRIMARY KEY,
    ORDER_DATE VARCHAR NOT NULL,
    DEPT_CODE VARCHAR NOT NULL,
    CUST_CODE VARCHAR NOT NULL,
    EMP_CODE VARCHAR NOT NULL,
    REQUIRED_DATE VARCHAR,
    CUSTORDER_NO VARCHAR,
    WH_CODE VARCHAR NOT NULL,
    ORDER_AMNT INTEGER NOT NULL DEFAULT 0,
    CMP_TAX INTEGER NOT NULL DEFAULT 0,
    SLIP_COMMENT VARCHAR NOT NULL,
    UPDATE_USER_NAME VARCHAR,
    USER_UPDATE_DATETIME VARCHAR DEFAULT CURRENT_TIMESTAMP
);

DELETE FROM
    ORDERS;

INSERT
INTO ORDERS (
    ORDER_NO, ORDER_DATE, DEPT_CODE, CUST_CODE, EMP_CODE, REQUIRED_DATE,
    CUSTORDER_NO, WH_CODE, ORDER_AMNT, CMP_TAX, SLIP_COMMENT,
    UPDATE_USER_NAME, USER_UPDATE_DATETIME
)
VALUES
(
    'OD1001',
    '2024-07-06',
    'D100',
    'C1001',
    'E002',
    NULL,
    NULL,
    '300',
    28000,
    2800,
    'P001,P002,P003',
    'SYSTEM',
    '2024-07-30 01:01:01'
),
(
    'OD1002',
    '2024-07-07',
    'D200',
    'C1001',
    'E003',
    NULL,
    NULL,
    '200',
    7200,
    720,
    'P101',
    'SYSTEM',
    '2024-07-30 01:01:01'
),
(
    'OD1003',
    '2024-07-08',
    'D200',
    'C1001',
    'E002',
    NULL,
    NULL,
    '100',
    25000,
    2500,
    'P20000,P20,1234',
    'SYSTEM',
    '2024-07-30 01:01:01'
),
(
    'OD1004',
    '2024-07-09',
    'D100',
    'C1001',
    'E002',
    NULL,
    NULL,
    '100',
    26400,
    2640,
    '1,2,3,4,5,6,7,8,9,10',
    'SYSTEM',
    '2024-07-30 01:01:01'
),
(
    'OD1005',
    '2024-07-10',
    'D100',
    'C1001',
    'E001',
    NULL,
    NULL,
    '100',
    24000,
    2400,
    '001,0002,0003,04',
    'SYSTEM',
    '2024-07-30 01:01:01'
);
