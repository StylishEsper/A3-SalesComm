       identification division.
       program-id. A3-SalesComm.
       author. Ahmed Butt.
       date-written. 2021-02-11.
      *Program Description: Generates a report based on a given data file.
      *
       environment division.
      *
       input-output section.
       file-control.
      *
           select sales-file
               assign to "../../../A3-SalesComm/A3.dat"
               organization is line sequential.

           select report-file
               assign to "../../../A3-SalesComm/A3-SalesComm.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd sales-file
           data record is sales-rec
           record contains 32 characters.
      *
       01 sales-rec.
         05 sr-sman-num pic 999.
         05 sr-name pic x(8).
         05 sr-sales pic 9(6).
         05 sr-min pic 9(6).
         05 sr-max pic 9(6).
         05 sr-rate pic 99v9(4).
      *
       fd report-file
           data record is report-line
           record contains 90 characters.
      *
       01 report-line pic x(92).
      *
       working-storage section.
      *
       01 ws-flags.
         05 ws-eof-flag pic x value space.
      *
       01 ws-heading1-name-line.
         05 filler pic x(66) value spaces.
         05 ws-info pic x(24) value "Ahmed Butt, Assignment 3".
      *
       01 ws-heading2-title.
         05 filler pic x(33) value spaces.
         05 ws-title pic x(23) value "SALES COMMISSION REPORT".
      *
       01 ws-heading3-headings.
         05 ws-columns pic x(92) value "NO.     NAME      SALES     MIN      MAX    RATE     EARNED        PAID     BONUS/NO BONUS".
      *
       01 ws-heading4-underlines.
         05 ws-underlines pic x(92) value "---   --------   -------  -------  -------  ------  ----------  ----------  ----------------".
      *
       77 ws-true-cnst pic x value "Y".
       77 ws-false-cnst pic x value "N".
      *
       77 ws-hundred-cnst pic 999 value 100.
       77 ws-threehundothou-cnst pic 9(6) value 300000.
       77 ws-bonusrate-cnst pic 9v9(4) value 0.1525.
       77 ws-one-cnst pic 9 value 1.
      *
       77 ws-yesbonus-cnst pic x(12) value "BONUS EARNED".
       77 ws-nobonus-cnst pic x(17) value "BONUS NOT EARNED".
      *
       01 ws-calc-help.
         05 ws-hold-earned pic 9(6).
         05 ws-hold-bonus pic 9(6).
         05 ws-totale-calc pic 9(7).
         05 ws-totalp-calc pic 9(7).
      *
       01 ws-total-disp.
         05 filler pic x(44) value spaces.
         05 filler pic x(6) value "Totals".
         05 filler pic x(2) value spaces.
         05 filler pic x value "$".
         05 ws-total-earned pic z,zzz,zz9.
         05 filler pic x(2) value spaces.
         05 filler pic x value "$".
         05 ws-total-paid pic z,zzz,zz9.
      *
       01 ws-line-page-counters.
         05 ws-line-count pic 999 value 0.
         05 ws-page-count pic 999 value 0.
         05 ws-lines-per-page-cnst pic 999 value 9.
      *
       01 ws-more-max.
         05 filler pic x(38) value "NUMBER WITH BONUS MORE THAN MAX       ".
         05 ws-num-max pic zz9.
      *
       01 ws-less-min.
         05 filler pic x(38) value "NUMBER WITH NO BONUS LESS THAN MIN    ".
         05 ws-num-min pic zz9.
      *
       01 ws-with-bonus.
         05 filler pic x(38) value "NUMBER OF SALESPEOPLE WITH BONUS      ".
         05 ws-num-with pic zz9.
      *
       01 ws-without-bonus.
         05 filler pic x(38) value "NUMBER OF SALESPEOPLE WITHOUT BONUS   ".
         05 ws-num-without pic zz9.
      *
       01 ws-num-salespeople.
         05 filler pic x(38) value "NUMBER OF SALESPEOPLE                 ".
         05 ws-num-sp pic zz9.
      *
       01 ws-num-paid-earned.
         05 filler pic x(38) value "NUMBER  WITH PAID EQUAL EARNED        ".
         05 ws-num-equal pic zz9.
      *
       01 ws-per-paid-earned.
         05 filler pic x(38) value "PERCENT WITH PAID EQUAL EARNED        ".
         05 ws-per-equal pic z99.99.
         05 filler pic x value "%".

       01 ws-hold-per pic 999v99999.
       01 ws-hold-num-without pic 999.
       01 ws-hold-num-sp pic 999.
       01 ws-hold-num-min pic 999.
       01 ws-hold-num-equal pic 999.
       01 ws-hold-num-with pic 999.
       01 ws-hold-num-max pic 999.
      *
       01 ws-per-with-bonus.
         05 filler pic x(38) value "PERCENT WITH BONUS     >300,000       ".
         05 ws-per-bonus pic z99.99.
         05 filler pic x value "%".
      *
       01 ws-per-without-bonus.
         05 filler pic x(38) value "PERCENT WITHOUT BONUS <=300,000       ".
         05 ws-per-nobonus pic z99.99.
         05 filler pic x value "%".
      *
       01 ws-detail-line.
         05 ws-sman-num pic 999.
         05 filler pic x(3) value spaces.
         05 ws-name pic x(8).
         05 filler pic x(3) value spaces.
         05 ws-sales pic zzz,zz9.
         05 filler pic x(2) value spaces.
         05 ws-min pic zzz,999.
         05 filler pic x(2) value spaces.
         05 ws-max pic zzz,999.
         05 filler pic x(2) value spaces.
         05 ws-rate pic z9.99.
         05 filler pic x(1) value "%".
         05 filler pic x(3) value spaces.
         05 ws-earned pic z,zzz,zz9.
         05 filler pic x(2) value spaces.
         05 filler pic x(1) value "$".
         05 ws-paid pic *,***,***.
         05 filler pic x(2) value spaces.
         05 ws-bonus pic x(16).
      *
       procedure division.
       000-main.
      *
           move ws-false-cnst to ws-eof-flag.

           open input sales-file.
           open output report-file.

           read sales-file
               at end
                   move ws-true-cnst to ws-eof-flag.

           perform 100-process-pages
             until ws-eof-flag = ws-true-cnst.

           move ws-totale-calc to ws-total-earned.
           move ws-totalp-calc to ws-total-paid.

           display "".
           display ws-total-disp.

           perform 800-print-totals.

           divide ws-hold-num-equal by ws-hold-num-sp giving ws-hold-per.
           multiply ws-hold-per by ws-hundred-cnst giving ws-per-equal rounded.

           divide ws-hold-num-with by ws-hold-num-sp giving ws-hold-per.
           multiply ws-hold-per by ws-hundred-cnst giving ws-per-bonus rounded.

           divide ws-hold-num-without by ws-hold-num-sp giving ws-hold-per.
           multiply ws-hold-per by ws-hundred-cnst giving ws-per-nobonus rounded.

           display "".
           display ws-more-max.
           display ws-less-min.
           display "".
           display "".
           display "".
           display ws-with-bonus.
           display ws-without-bonus.
           display ws-num-salespeople.
           display "".
           display "".
           display "".
           display ws-num-paid-earned.
           display ws-per-paid-earned.
           display "".
           display "".
           display "".
           display ws-per-with-bonus.
           display ws-per-without-bonus.

           perform 400-bonus-greater-than.
           perform 500-bonus-less-than.
           perform 600-bonus-under-minimum.
           perform 700-bonus-over-maximum.

           close sales-file, report-file.

           accept return-code.

           goback.
      *
       100-process-pages.
      *
           move 0 to ws-line-count.

           perform 200-print-headings

           perform 300-process-lines
             until ws-eof-flag = ws-true-cnst
             OR ws-line-count > ws-lines-per-page-cnst.
      *
       200-print-headings.
      *
           add ws-one-cnst to ws-page-count.

           if ws-page-count = ws-one-cnst
               display ws-heading1-name-line
               write report-line from ws-heading1-name-line
           else
               display ""
               display ""
               display ws-heading1-name-line
               write report-line from ""
               write report-line from ""
               write report-line from ws-heading1-name-line after advancing page
           end-if.
      *
           display "".
           display ws-heading2-title.
           display "".
           display ws-heading3-headings.
           display ws-heading4-underlines.

           write report-line from ws-heading2-title after advancing 1 line.
           write report-line from ws-heading3-headings after advancing 1 line.
           write report-line from ws-heading4-underlines.
      *
       300-process-lines.
      *
           move sr-sman-num to ws-sman-num.
           move sr-name to ws-name.
           move sr-sales to ws-sales.
           move sr-min to ws-min.
           move sr-max to ws-max.
           move sr-rate to ws-rate.

           add ws-one-cnst to ws-hold-num-sp.
           move ws-hold-num-sp to ws-num-sp.

           if sr-sales <= ws-threehundothou-cnst
             then
               add ws-one-cnst to ws-hold-num-without
               move ws-hold-num-without to ws-num-without
               divide sr-rate by ws-hundred-cnst giving sr-rate
               multiply sr-sales by sr-rate giving ws-hold-earned rounded
               move ws-hold-earned to ws-earned
               move ws-nobonus-cnst to ws-bonus
               move ws-earned to ws-paid
               add ws-one-cnst to ws-hold-num-equal
               move ws-hold-num-equal to ws-num-equal
               add ws-hold-earned to ws-totale-calc
               if ws-hold-earned < sr-min
                 then
                   move ws-min to ws-paid
                   subtract ws-one-cnst from ws-hold-num-equal
               move ws-hold-num-equal to ws-num-equal
                   add sr-min to ws-totalp-calc
                   add ws-one-cnst to ws-hold-num-min
                   move ws-hold-num-min to ws-num-min
               else
                   add ws-hold-earned to ws-totalp-calc
               end-if
           end-if

           if sr-sales > ws-threehundothou-cnst
             then
               add ws-one-cnst to ws-hold-num-with
               move ws-hold-num-with to ws-num-with
               divide sr-rate by ws-hundred-cnst giving sr-rate
               multiply sr-sales by sr-rate giving ws-hold-earned
               subtract ws-threehundothou-cnst from sr-sales giving sr-sales
               multiply sr-sales by ws-bonusrate-cnst giving ws-hold-bonus
               add ws-hold-earned to ws-hold-bonus giving ws-earned rounded
               move ws-yesbonus-cnst to ws-bonus
               move ws-earned to ws-paid
               add ws-one-cnst to ws-hold-num-equal
               move ws-hold-num-equal to ws-num-equal
               add ws-hold-earned to ws-totale-calc
               if ws-hold-earned > sr-min
                 then
                   move ws-max to ws-paid
                   subtract ws-one-cnst from ws-hold-num-equal
                   move ws-hold-num-equal to ws-num-equal
                   add sr-max to ws-totalp-calc
                   add ws-one-cnst to ws-hold-num-max
                   move ws-hold-num-max to ws-num-max
               else
                   add ws-hold-earned to ws-totalp-calc
               end-if
           end-if

           display ws-detail-line.
           display "".

           write report-line from ws-detail-line.
           write report-line from ""

           add ws-one-cnst to ws-line-count.

           read sales-file
               at end
                   move ws-true-cnst to ws-eof-flag.

      *
       400-bonus-greater-than.
      *
           write report-line from "".
           write report-line from ws-more-max.
      *
       500-bonus-less-than.
      *
           write report-line from ws-less-min.
           write report-line from "".
           write report-line from "".
           write report-line from "".
      *
       600-bonus-under-minimum.
      *
           write report-line from ws-with-bonus.
           write report-line from ws-without-bonus.
           write report-line from ws-num-salespeople.
           write report-line from "".
           write report-line from "".
           write report-line from "".
      *
       700-bonus-over-maximum.
      *
           write report-line from ws-num-paid-earned.
           write report-line from ws-per-paid-earned.
           write report-line from "".
           write report-line from ws-per-with-bonus.
           write report-line from ws-per-without-bonus.
      *
       800-print-totals.
      *
           write report-line from "".
           write report-line from ws-total-disp.
      *
       end program A3-SalesComm.