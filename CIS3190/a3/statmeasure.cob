      *> Luc Levesque
      *> 1238403
      *> statmeasure.cob
      *> modernized version of statmold.cob
       identification division.
       program-id. statmeasure2.

       environment division.
       input-output section.
       file-control.
           select input-file assign to ws-filename
               organization is line sequential
               file status is ws-file-status.

       data division.
       file section.
       fd input-file.
       01 input-record pic x(80).

       working-storage section.
       01 ws-filename pic x(256).
       01 ws-file-status pic xx.
       01 ws-end-of-file-flag pic 9 value 0.
           88 end-of-file value 1.

       01 ws-data-count pic s9(4) value 0.
       01 ws-index pic s9(4).

      *> array for storing data values
       01 data-array.
           02 data-value pic s9(6)v9(2)
               occurs 1000 times.


      *> arithmetic mean variables
       01 ws-sum-of-values pic s9(10)v9(2) value 0.
       01 ws-arithmetic-mean pic s9(6)v9(2).

      *> standard deviation variables
       01 ws-sum-squared-diff pic 9(14)v9(6) value 0.
       01 ws-standard-dev pic s9(6)v9(2).

      *> geometric mean variables (log-based)
       01 ws-sum-of-logarithms pic s9(10)v9(8) value 0.
       01 ws-logarithm-value pic s9(10)v9(8).
       01 ws-geometric-mean pic s9(6)v9(2).

      *> harmonic mean variables
       01 ws-sum-of-reciprocals pic s9(10)v9(8) value 0.
       01 ws-harmonic-mean pic s9(6)v9(2).

      *> root mean square variables
       01 ws-sum-of-squares pic 9(14)v9(6) value 0.
       01 ws-root-mean-square pic s9(6)v9(2).

      *> input parsing and display variables
       01 ws-decimal-count pic 9 value 0.
       01 ws-display-value pic -(6)9.9(2).

       procedure division.
       main-program.
           perform get-filename
           perform read-data
           if ws-data-count > 0
               perform display-header
               perform display-data
               perform calculate-mean
               perform calculate-standard-dev
               perform calculate-geometric-mean
               perform calculate-harmonic-mean
               perform calculate-root-mean-square
               perform display-results
           else
               display "no data values found in file."
           end-if
           stop run.

      *> prompt user for input filename
       get-filename.
           display "Enter the input filename: "
               with no advancing
           accept ws-filename
           display spaces.

      *> read all values from file into data array
       read-data.
           open input input-file
           if ws-file-status not = "00"
               display "error: cannot open file '"
                   ws-filename "'"
               stop run
           end-if
           move 0 to ws-data-count
           move 0 to ws-end-of-file-flag
           perform until end-of-file
               read input-file
                   at end
                       set end-of-file to true
                   not at end
                       perform process-input-record
               end-read
           end-perform
           close input-file.

      *> auto-detect format: if no decimal point, divide by 100
       process-input-record.
           if input-record not = spaces
               add 1 to ws-data-count
               move 0 to ws-decimal-count
               inspect input-record
                   tallying ws-decimal-count
                   for all "."
               if ws-decimal-count = 0
                   compute data-value(ws-data-count) =
                       function numval(input-record)
                       / 100
               else
                   compute data-value(ws-data-count) =
                       function numval(input-record)
               end-if
           end-if.

       display-header.
           display "==============================="
           display "  Statistical Measures Report"
           display "==============================="
           display " "
           display "   Data Values"
           display "   -----------".

       display-data.
           perform varying ws-index from 1 by 1
               until ws-index > ws-data-count
               move data-value(ws-index)
                   to ws-display-value
               display "   " ws-display-value
           end-perform
           display " ".

      *> mean = sum(x_i) / n
       calculate-mean.
           move 0 to ws-sum-of-values
           perform varying ws-index from 1 by 1
               until ws-index > ws-data-count
               compute ws-sum-of-values =
                   ws-sum-of-values +
                   data-value(ws-index)
           end-perform
           compute ws-arithmetic-mean rounded =
               ws-sum-of-values / ws-data-count.

      *> std dev = sqrt(sum((x_i - mean)^2) / n)
       calculate-standard-dev.
           move 0 to ws-sum-squared-diff
           perform varying ws-index from 1 by 1
               until ws-index > ws-data-count
               compute ws-sum-squared-diff =
                   ws-sum-squared-diff +
                   (data-value(ws-index) -
                   ws-arithmetic-mean) ** 2
           end-perform
           compute ws-standard-dev rounded =
               (ws-sum-squared-diff /
               ws-data-count) ** 0.5.

      *> geometric mean = exp(sum(ln(x_i)) / n)
       calculate-geometric-mean.
           move 0 to ws-sum-of-logarithms
           perform varying ws-index from 1 by 1
               until ws-index > ws-data-count
               compute ws-logarithm-value =
                   function log(
                   data-value(ws-index))
               compute ws-sum-of-logarithms =
                   ws-sum-of-logarithms +
                   ws-logarithm-value
           end-perform
           compute ws-geometric-mean rounded =
               function exp(
               ws-sum-of-logarithms /
               ws-data-count).

      *> harmonic mean = n / sum(1/x_i)
       calculate-harmonic-mean.
           move 0 to ws-sum-of-reciprocals
           perform varying ws-index from 1 by 1
               until ws-index > ws-data-count
               compute ws-sum-of-reciprocals =
                   ws-sum-of-reciprocals +
                   (1 / data-value(ws-index))
           end-perform
           compute ws-harmonic-mean rounded =
               ws-data-count /
               ws-sum-of-reciprocals.

      *> rms = sqrt(sum(x_i^2) / n)
       calculate-root-mean-square.
           move 0 to ws-sum-of-squares
           perform varying ws-index from 1 by 1
               until ws-index > ws-data-count
               compute ws-sum-of-squares =
                   ws-sum-of-squares +
                   data-value(ws-index) ** 2
           end-perform
           compute ws-root-mean-square rounded =
               (ws-sum-of-squares /
               ws-data-count) ** 0.5.

       display-results.
           display "==============================="
           display "          Results"
           display "==============================="
           move ws-sum-of-values to ws-display-value
           display "   Sum               = " ws-display-value
           move ws-arithmetic-mean to ws-display-value
           display "   Mean              = " ws-display-value
           move ws-standard-dev to ws-display-value
           display "   Standard Dev      = " ws-display-value
           move ws-geometric-mean to ws-display-value
           display "   Geometric mean    = " ws-display-value
           move ws-harmonic-mean to ws-display-value
           display "   Harmonic mean     = " ws-display-value
           move ws-root-mean-square to ws-display-value
           display "   rms               = " ws-display-value
           display "===============================".
