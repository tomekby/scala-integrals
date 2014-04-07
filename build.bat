@ECHO off
REM kasownie w razie potrzeby
SET NAME=integrals
CD dist
IF EXIST %NAME%.jar DEL %NAME%.jar
CD ../build/classes
IF EXIST %NAME%.jar DEL %NAME%.jar
REM kopiowanie potrzebnych plików
IF NOT EXIST scala-library.jar COPY "%SCALA_HOME%"\lib\scala-library.jar .
IF NOT EXIST scala-swing.jar COPY "%SCALA_HOME%"\lib\scala-swing.jar .
REM kompilacja
jar -cfm %NAME%.jar ../../manifest.mf backend\*.class frontend\*.class
REM przenoszenie do dist/
IF NOT EXIST ../../scala-library.jar MOVE scala-library.jar ../../dist/scala-library.jar
IF NOT EXIST ../../scala-library.jar MOVE scala-swing.jar ../../dist/scala-swing.jar
MOVE %NAME%.jar ../../dist/%NAME%.jar
CD ../../dist
java -jar %NAME%.jar
CD ..
PAUSE