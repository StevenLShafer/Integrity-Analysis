@echo off
rem Launch the corpus/TEST mass analysis with the CORRECT R (4.5.3 - the
rem default Rscript on PATH is R 4.6, whose library lacks the package).
cd /d "%~dp0.."
"C:\Program Files\R\R-4.5.3\bin\Rscript.exe" corpus\runMassTest.R %*
pause
