module ExampleCode (importFile)
where

import System.IO ( hClose, hIsEOF, openFile, hGetLine, Handle, IOMode(ReadMode) )
import Database.HDBC
import Database.HDBC.ODBC


importFile :: IO ()
importFile = do
    conn <- connectODBC "Driver={ODBC Driver 17 for SQL Server};Server=xxx;Database=xxx;Trusted_Connection=yes;"
    inh <- openFile "Inputs/03_input.txt" ReadMode
    processInput conn inh
    commit conn
    disconnect conn

processInput conn h = do
    eof <- hIsEOF h
    if eof then print ""
    else do
        x <- hGetLine h
        --execute stmt [toSql x]
        run conn "insert into dbo.cdp3 (a) values (?)" [toSql x]
        processInput conn h


