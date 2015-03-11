fload FileID1.f


create cyn  1024 allot

: test ( -- )

  s" 4743_20130503.txt" fid1 檔名 + lplace
 
  開檔1

\  讀檔1
  
\  fid1 檔案資料 + fid1 檔案讀取長度 + @ dump
  
  cr
  100 0 do
  cyn 1024 fid1 檔頭序號 + @ read-line drop drop
  cyn swap type cr  
  loop
  ;
  
  test
