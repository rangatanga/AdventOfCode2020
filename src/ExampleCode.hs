module ExampleCode (importFile)
where

import System.IO ( hClose, hIsEOF, openFile, hGetLine, Handle, IOMode(ReadMode) )
import Database.HDBC
import Database.HDBC.ODBC


importFile :: IO ()
importFile = do
    conn <- connectODBC "Driver={ODBC Driver 17 for SQL Server};Server=DESKTOP-92A88D7;Database=BetFair;Trusted_Connection=yes;"
    inh <- openFile "Inputs/04_input.txt" ReadMode
    processInput conn inh
    commit conn
    disconnect conn

processInput conn h = do
    eof <- hIsEOF h
    if eof then print ""
    else do
        x <- hGetLine h
        --execute stmt [toSql x]
        run conn "insert into dbo.cdp4 (a) values (?)" [toSql x]
        processInput conn h


