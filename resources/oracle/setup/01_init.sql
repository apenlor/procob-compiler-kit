--
-- Oracle Database Initialization Script
--
-- This script is executed when the database container is first created.
-- It sets up the necessary user, schema, and tables for the Pro*COBOL application.
--

-- Switch to the correct Pluggable Database (PDB)
-- The default PDB for the 'database/free' image is FREEPDB1.
ALTER SESSION SET CONTAINER = FREEPDB1;

-- Create a dedicated user for the application
CREATE USER procob IDENTIFIED BY procob;

-- Grant necessary permissions
GRANT CONNECT, RESOURCE, UNLIMITED TABLESPACE TO procob;

-- Switch to the new user's schema for subsequent commands
ALTER SESSION SET CURRENT_SCHEMA = procob;

-- Create the EURIBOR table
CREATE TABLE EURIBOR (
    YEAR      NUMBER(4)    NOT NULL,
    MONTH     NUMBER(2)    NOT NULL,
    INT_RATE  NUMBER(4,3)  NOT NULL,
    CONSTRAINT EURIBOR_PK PRIMARY KEY (YEAR, MONTH)
);

-- Insert initial data
INSERT INTO EURIBOR VALUES (2025, 12, 0.023);
INSERT INTO EURIBOR VALUES (2026, 01, 0.024);
INSERT INTO EURIBOR VALUES (2026, 02, 0.025);
INSERT INTO EURIBOR VALUES (2026, 03, 0.026);
INSERT INTO EURIBOR VALUES (2026, 04, 0.027);
INSERT INTO EURIBOR VALUES (2026, 05, 0.028);
INSERT INTO EURIBOR VALUES (2026, 06, 0.029);
INSERT INTO EURIBOR VALUES (2026, 07, 0.030);
INSERT INTO EURIBOR VALUES (2026, 08, 0.031);

-- Commit the transaction
COMMIT;
