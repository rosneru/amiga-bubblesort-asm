*======================================================================
* Sortiert eine Datei mit dem BubbleSort-Verfahren. Die Datei muss
* zeilenweise integer-Zahlen enthalten. Die benötigten Zeiten für das
* Laden, Sortieren etc werden gemessen und ausgegeben. Bei der Angabe
* des optionalen Argumentes P bzw. PRINT werden vor der Ausgabe der
* Statistik die Zahlen in sortierter Reihenfolge ausgegeben.
*
* Entwicklungszeitraum:
*   12.05.2019 - 00.00.0000 - Optimierte Version
*   24.04.2018 - 12.05.2018 - Basisversion
*
* Autor: Uwe Rosner
*    https://github.com/rosneru
*======================================================================

        include   exec/exec_lib.i
        include   exec/execbase.i
        include   exec/io.i
        include   exec/memory.i
        include   exec/ports.i
        include   dos/dos_lib.i
        include   dos/dos.i
        include   dos/dosextens.i
        include   devices/timer.i
        include   lvo/timer.i
        include   lvo/mathieeesingbas.i

_SYSBase = 4

OS_VERSION = 37                           ;Version der Libraries etc
                                          ;muss min. OS2.04 sein

LINE_LEN = 80                             ;Länge Zeilenpuffer

*======================================================================
* Initialisierungen
*======================================================================

        lea       bDosReturnErr(pc),a0  ;Standardmäßig Programm ohne
        move.b    #0,(a0)               ;Dos-Fehlercode beenden

        move.l    _SYSBase,a6           ;exec.library benutzen

;
;MessagePort und IOrequest erzeugen
;           
        jsr     _LVOCreateMsgPort(a6)   ;MessagePort erstellen
        lea     MessagePort(pc),a0      ;Adresse der Variable für MP
        move.l  d0,(a0)                 ;Adresse des MP in Variable merken
                
        move.l  d0,a0                   ;MP-Adresse nun nach a0
        moveq   #IO_SIZE,d0             ;Speichergröße für timerequest
        addq    #EV_SIZE,d0             ;ist Größe IORequest + EClockVal
        jsr     _LVOCreateIORequest(a6) ;struct timerequest erzeugen
        lea     TimerIO(pc),a0          ;Adresse der Variable für IOReq
        move.l  d0,(a0)                 ;Adresse von IOReq in Variable merken

;
;timer.device öffnen
;
        lea     timername(pc),a0        ;Zeiger auf Name des timer.device
        lea     TimerIO(pc),a1    
        move.l  (a1),a1   
        moveq   #UNIT_ECLOCK,d0         ;EClock zählen
        moveq   #TR_GETSYSTIME,d1       ;FLags nach d1
        jsr     _LVOOpenDevice(a6)      ;timer.device öffnen
    
        tst.l   d0                      ;Fehler bei OpenDevice?
        beq     initDosLib              ;wenn ok
    
        lea     bDosReturnErr(pc),a0    ;Fehler: am Programmende einen
        move.b  #1,(a0)                 ;Dos-Fehlercode setzen
    
        bra     disposeTimeRequest      ;Ausstieg dort beginnen

;
; dos.library öffnen
;
initDosLib
        lea     dosname(pc),a1          ;Name der DOS-Lib
        moveq   #OS_VERSION,d0          ;benötigte Version
        jsr     _LVOOpenLibrary(a6)     ;DOS-Lib öffnen
        move.l  d0,_DOSBase             ;Zeiger merken
        tst.l   d0                      ;Fehler?
        bne.s   initMathLib             ;wenn ok
    
        lea     bDosReturnErr(pc),a0    ;Fehler: am Programmende einen
        move.b  #1,(a0)                 ;Dos-Fehlercode setzen
    
        bra     disposeTimerDev         ;Ausstieg dort beginnen

;
; mathieeesingbas.library öffnen
;
initMathLib
        lea     mathname(pc),a1         ;Name der Lib
        moveq   #OS_VERSION,d0          ;benötigte Version
        jsr     _LVOOpenLibrary(a6)     ;DOS-Lib öffnen
        move.l  _DOSBase,a6             ;dos.library wird gebraucht
        move.l  d0,_MATHBase            ;Zeiger merken
        tst.l   d0                      ;Fehler?
        bne.s   initArgs                ;wenn ok
    
        move.l  #strErrOpMathLib,d1     ;Fehlertext nach d1
        move.l  0,d2                    ;kein Argument
        jsr     _LVOVPrintf(a6)         ;formatiert ausgeben
    
        bra     disposeDosLib           ;Ausstieg dort beginnen

;
; Argumente abfragen
;
initArgs
        lea     argTemplate(pc),a0      ;Argument-Vorgabe laden
        move.l  a0,d1                   ;für Aufruf nach d1
        move.l  #argArray,d2            ;Argument-Feld nach d2
        move.l  #0,d3                   ;keine ReadArgs-Struktur
        move.l  _DOSBase,a6             ;verwende die dos.library
        jsr     _LVOReadArgs(a6)        ;Aufruf ReadArgs(d1,d2,d3)
        move.l  d0,d6                   ;RDArgs-Struktur retten
        bne.s   main                    ;bei Erfolg dort weiter
    
        pea     disposeMathLib(pc)      ;nachher Ausstieg dort beginnen
        bra     PrintDosErr             ;DOS-Fehler ausgeben und Ausstieg


*======================================================================
* Hauptprogramm
*======================================================================
main
        move.l  #-666,lineCount ;TODO Dummy; wieder weg
        move.l  #-1,timeCounting ;TODO Dummy; wieder weg
        move.l  #-1,timeLoading  ;TODO Dummy; wieder weg
        move.l  #-1,timeSorting  ;TODO Dummy; wieder weg

;
; Datei öffnen
;

        move.l  argArray,d1             ;Dateiname holen; ist gleich der
                                        ;erste Eintrag im ArgArray

        move.l  #MODE_OLDFILE,d2        ;Modus "bestehende Datei öffnen"
        jsr     _LVOOpen(a6)            ;Datei öffnen (dos.library)

        lea     inputHandle(pc),a0
        move.l  d0,(a0)                 ;Datei-Handle merken
        tst.l   d0                      ;Fehler?
        bne.s   mainCountLines          ;bei Erfolg dort weiter
    
        pea     disposeArgs(pc)         ;nachher Ausstieg dort beginnen
        bra     PrintDosErr             ;DOS-Fehler ausgeben und Ausstieg

mainCountLines
;
; Anzahl der Zeilen in der Datei ermitteln
;
        bsr     TimerStart              ;Timer starten
    
        lea     inputHandle(pc),a2      ;Adresse InputHandle-Variable vorladen
        lea     lineBuf(pc),a3          ;Adresse lineBuf vorladen
        move.l  a3,d2                   ;lineBuf

        moveq   #LINE_LEN,d3            ;Zeilenlänge
        subq    #1,d3                   ;OS2.0-Workaround: LINE_LEN-1


        sub.l   a5,a5                   ;a5 löschen
        subq.l  #1,a5                   ;a5 auf -1 setzen; Zeilenzähler
    
main_cl_loop    
        addq.l  #1,a5                   ;Zeilenzähler inkrementieren
            
        move.l  (a2),d1                 ;FileHandle Eingabedatei
        jsr     _LVOFGets(a6)           ;Zeile aus Datei lesen

        tst.l   d0                      ;Konnte noch lesen?
        bne.s   main_cl_loop            ;Ja, weiter lesen
    
        bsr     TimerStop               ;Timer stoppen, timeout berechnen
        move.l  d0,timeCounting         ;timeout in Variable merken
    
        lea     lineCount(pc),a0
        move.l  a5,(a0)                 ;Zeilenanzahl in lineCount merken

main_allocAllLinesBuf
;
; Speicher reservieren alle int (hier Longword) - Werte der Eingabedatei
;

        move.l  a5,d0                   ;lineCount Langworte reservieren,
        mulu    #4,d0                   ;d.h. lineCount * 4 Bytes
        move.l  #MEMF_CLEAR,d1          ;beliebiger Speicher, aber löschen
        move.l  _SYSBase,a6             ;exec.library verwenden
        jsr     _LVOAllocVec(a6)        ;Speicher reservieren
        move.l  _DOSBase,a6             ;ab jetzt wieder die dos.library
        lea     allLinesBuf(pc),a0
        move.l  d0,(a0)                 ;Adresse des Zeilenpuffers merken
        tst.l   d0                      ;Fehler?
        bne.s   mainLoadLines           ;bei Erfolg dort weiter
    
        move.l  #strErrAllocVec,d1      ;Fehlertext nach d1
        move.l  0,d2                    ;kein Argument
        jsr     _LVOVPrintf(a6)         ;formatiert ausgeben
    
        bra     disposeArgs             ;Ausstieg ab dort

mainLoadLines
;
;TODO Zeilen aus Datei als (long-)Werte in ein Array laden
;

;Lesezeiger in Datei zurücksetzen
        move.l  inputHandle(pc),d1      ;FileHandle der Eingabedatei
        moveq   #0,d2                   ;neue Position in File ist 0..
        moveq   #OFFSET_BEGINNING,d3    ;vom Anfang
        jsr     _LVOSeek(a6)            ;Seek (dos.library)

        lea     inputHandle(pc),a2      ;Adresse InputHandle-Variable vorladen

        lea     lineBuf(pc),a3          ;Adresse lineBuf-Variable vorladen
        move.l  a3,d2                   ;lineBuf

        moveq   #LINE_LEN,d3            ;Zeilenlänge
        subq    #1,d3                   ;OS2.0-Workaround: LINE_LEN-1

        lea     allLinesBuf(pc),a4      ;a4 zeigt auf Adresse der Puffervariable
        move.l  (a4),a4                 ;a4 zeigt nun auf Pufferanfang


        move.l  a5,d5                   ;lineCount nach d5 (Zählregister)
        subq.l  #1,d5                   ;d5 mit lineCount-1 initialisieren

        bsr TimerStart                  ;Timer starten

;Puffer initilisieren
main_ll_loop
        move.l  (a2),d1                 ;inputHandle
        jsr     _LVOFGets(a6)           ;Zeile aus Datei lesen

        move.l  a3,a0                   ;Parameter für decin ist lineBuf
        bsr     decin                   ;lineBuf (a0) in int wandeln

        move.l  d1,(a4)+                ;Ergebnis in Puffer schreiben und a4
                                        ;auf nächstes Pufferelement zeigen lassen
        dbra    d5,main_ll_loop

        bsr     TimerStop               ;Timer stoppen, timeout berechnen
        move.l  d0,timeLoading          ;timeout in Variable merken

;
;Datei schließen
;
        move.l  inputHandle,d1
        jsr     _LVOClose(a6)           ;Datei schließen


;
;Array sortieren
;
        bsr     TimerStart              ;Timer starten

        lea     allLinesBuf(pc),a0      ;a0 zeigt auf Adresse der Puffervariable
        move.l  (a0),a0                 ;a0 zeigt nun auf Pufferanfang
        lea     lineCount(pc),a1
        move.l  (a1),d0                 ;Anzahl Longwords im Puffer nach d0
        bsr     BubbleSort              ;Puffer a0 sortieren

        bsr     TimerStop               ;Timer stoppen, timeout berechnen
        move.l  d0,timeSorting          ;timeout in Variable merken

;
;Sortiertes Array ausgeben, wenn P oder PRINT als
;Kommandozeilenargument übergeben wurden
;
mainPrintVal
        lea     argArray(pc),a0         ;Argument-Feld nach a0
        add.l   #4,a0                   ;a0 soll auf nächste Adresse zeigen (Arg[2])
        move.l  (a0),d0                 ;Inhalt von Arg[2] nach d0 kopieren
        tst.l   d0                      ;wenn Inhalt 0 ist
        beq.s   mainPrintStat           ;sortierte Werte nicht ausgeben

        lea     lineCount(pc),a1
        move.l  (a1),d5                 ;lineCount nach d5 (Zählregister)
        subq.l  #1,d5                   ;d5 mit lineCount-1 initialisieren

        lea     allLinesBuf(pc),a4      ;a4 zeigt auf Adresse der Puffervariable
        move.l  (a4),a4                 ;a4 zeigt auf Anfang des Inhaltspuffers

main_pv_loop

        move.l  a4,d2                   ;Argument ist Adresse der Zahl
        move.l  #strFmtNumber,d1        ;FormatString für Aufruf nach d1
        jsr     _LVOVPrintf(a6)         ;VPrintf der dos.library aufrufen

        addq.l  #4,a4

        dbra    d5,main_pv_loop





;
; Statistik über die benötigten Zeiten etc ausgben
;
mainPrintStat
        bsr     PrintStatistics

*======================================================================
* Speicher freigeben, Libraries schließen
*======================================================================

* Speicher für Dateiinhalt freigeben
disposeMem
        lea     allLinesBuf(pc),a1
        move.l  (a1),a1                 ;Pufferadresse Dateiinhalt
        move.l  _SYSBase,a6             ;exec.library
        jsr     _LVOFreeVec(a6)         ;freigeben

* RDArgs-Struktur freigeben
disposeArgs
        move.l    d6,d1                 ;gerettete RDArgs TODO: Variable statt d6. d6 kann weg sein..
        move.l  _DOSBase,a6             ;nochmal kurz die dos.library
        jsr     _LVOFreeArgs(a6)        ;RDArgs-Struktur wieder freigeben

* mathieeesingbas.library schließen
disposeMathLib
        move.l  _MATHBase,a1            ;Adresse dos.library
        move.l  _SYSBase,a6             ;ab jetzt nur noch exec.library
        jsr     _LVOCloseLibrary(a6)    ;dos.library schließen

* dos.library schließen
disposeDosLib
        move.l  _DOSBase,a1             ;Adresse dos.library
        move.l  _SYSBase,a6             ;ab jetzt nur noch exec.library
        jsr     _LVOCloseLibrary(a6)    ;dos.library schließen

* timer.device schließen
disposeTimerDev
        lea     TimerIO(pc),a1          ;Start-Adresse von IOReq
        jsr     _LVOCloseDevice(a6)     ;timer.device schließen

* Speicher für struct timerequest freigeben
disposeTimeRequest
        lea     TimerIO(pc),a0
        move.l  (a0),a0
        jsr     _LVODeleteIORequest(a6)

disposeMsgPort
        lea     MessagePort(pc),a0
        move.l  (a0),a0
        jsr     _LVODeleteMsgPort(a6)

        lea     bDosReturnErr(pc),a0
        move.b  (a0),d0
        tst.b   d0                      ;testen, ob bDosReturnErr gesetzt
        beq.s   fini                    ;nein: dann zum Ende springen

        moveq   #ERROR_INVALID_RESIDENT_LIBRARY,d0 ;ja: Fehlercode setzen
        movea.l ThisTask(a6),a0         ;Prozess-Struct der eigenen Task
        move.l  d0,pr_Result2(a0)       ;Fehlerursache eintragen

fini
        move.l  d4,d0                   ;Returncode fürs CLI
        rts                             ;return zum CLI

*======================================================================
* Unterprogramme für Zeitmessung
*
* Rückgabe: Anzahl ms zwischen TimerStart und TimerStop id d0.
*======================================================================
 
TimerStart
        movem.l a6,-(sp)                ;a6 retten
        
        lea     EClockStart(pc),a0      ;a0 mit Adresse von Variable
                                        ;EClockStart laden, dort wird der
                                        ;EClock-Wert gespeichert

        lea     TimerIO(pc),a1          ;struct timerequest
        move.l  (a1),a1   
        move.l  IO_DEVICE(a1),a6        ;timer.device als library benutzen
        jsr     _LVOReadEClock(a6)      ;ReadEClock() aus timer.device

        lea     clocksPerSecond(pc),a0
        move.l  d0,(a0)                 ;System-clocks pro Sekunde merken

        movem.l (sp)+,a6                ;a6 wieder herstellen
        rts

TimerStop
        movem.l a6,-(sp)                ;a6 retten
        
        lea     EClockStop(pc),a0       ;a0 mit Adresse von Variable
                                        ;EClockStop laden, dort wird der
                                        ;EClock-Wert gespeichert
                                        
        lea     TimerIO(pc),a1          ;struct timerequest
        move.l  (a1),a1
        move.l  IO_DEVICE(a1),a6        ;timer.device als library benutzen
        jsr     _LVOReadEClock(a6)      ;ReadEClock() aus timer.device

        move.l  _MATHBase,a6            ;verwende mathieeesingbas.library

        lea     clocksPerSecond(pc),a0  
        move.l  (a0),d0
        jsr     _LVOIEEESPFlt(a6)       ;clocksPerSecond in IEEE Single
        move.l  d0,d2                   ;konvertieren und in d2 merken

        move.l  #1000,d0                ;Wert 1000 nach d0
        jsr     _LVOIEEESPFlt(a6)       ;in IEEE Single konvertieren
        move.l  d0,d3                   ;und für später in d3 merken

        lea     EClockStart,a0    
        move.l  EV_LO(a0),d0            ;ev_lo des Startzeitpunktes
        jsr     _LVOIEEESPFlt(a6)       ;in IEEE Single konvertieren
        move.l  d0,d1                   ;und nach d1 kopieren
    
        lea     EClockStop,a0   
        move.l  EV_LO(a0),d0            ;ev_lo des Stopzeitpunktes holen
        jsr     _LVOIEEESPFlt(a6)       ;in IEEE-Single konvertieren

        jsr     _LVOIEEESPSub(a6);      ;d0 = d0 - d1
    
        move.l  d2,d1                   ;clocksPerSecond nach d1 holen
        jsr     _LVOIEEESPDiv(a6)       ;d0 = d0 / d1
    
        move.l  d3,d1                   ;in d3 gemerkten Wert 1000 nach d1
        jsr     _LVOIEEESPMul(a6)       ;d0 = d0 * d1
    
        jsr     _LVOIEEESPFix(a6)       ;Ergebnis wieder in den long-
                                        ;Zahlenbereich konvertieren (in d0)
    
        movem.l (sp)+,a6                ;a6 wieder herstellen

        rts

*======================================================================
* Unterprogramm BubbleSort
* sortiert einen Puffer von Longword-Werten aufsteigend.
*
* Parameter: A0 -> zeigt auf erstes Pufferelement.
*            D0 -> enthält Pufferlänge.
*
* Rückgabe: /
*======================================================================
BubbleSort
;Vorbereitungen
        movem.l a2-a4/d2-d5,-(sp)       ;register retten

        moveq   #0,d1                   ;Init d1 = äußerer Zähler k
                                        ;zunächst 0; wird in bs_k_loop zu 1 etc

        moveq   #0,d2                   ;Init d2 = innerer Zähler i
        subq.l  #1,d2                   ;d2 = d2 - 1
                                        ;zunächst -1; wird in bs_loop zu 0 etc

        move.l  a0,a1                   ;a1 = a0 (erstes Pufferelement)
        subq.l  #4,a1                   ;a1 = a1 - 4
                                        ;a1 zeigt zunächst auf Element i-1

        move.l  a0,a2                   ;a2 zeigt zunächst auf Element i

bs_k_loop
;Bedingung für i testen; wenn wahr bs_i_loop
;Bedingung für i ist: i < dataLength - k
        move.l  d0,d3                   ;dataLength nach d3
        sub.l   d1,d3                   ;d3 = dataLength - k
        cmp.l   d3,d2                   ;d2 (i) <=vgl=> d3 (dataLength)
        blt.s   bs_i_loop               ;Bedingung 'i < dataLenght-k' erfüllt

;nicht erfüllt:

;Bedingung für k testen; wenn falsch bs_fin
;Bedingung für k ist: k < dataLength
        cmp.l   d0,d1
        bge.s   bs_fin                  ;wenn Bedingung erfüllt

        addq.l  #1,d1                   ;k = k + 1

        moveq   #0,d2                   ;Init d2 = innerer Zähler i
        subq.l  #1,d2                   ;d2 = d2 - 1
                                        ;zunächst -1; wird in bs_loop zu 0 etc

        move.l  a0,a1                   ;a1 = a0 (erstes Pufferelement)
        subq.l  #4,a1                   ;a1 = a1 - 4
                                        ;a1 zeigt zunächst auf Element i-1

        move.l  a0,a2                   ;a2 zeigt zunächst auf Element i

bs_i_loop

        addq.l  #1,d2                   ;i = i + 1

        addq.l  #4,a1                   ;a1 zeigt danach auf Element i
        addq.l  #4,a2                   ;a2 zeigt danach auf Element i+1

        move.l  (a1),d3                 ;(i) in d3 zwischenspeichern
        cmp.l   (a2),d3                 ;Vergleich von (i) und (i+1)
        ble.s   bs_k_loop               ;wenn (i) <= (i+1)

                                        ;sonst umkopieren
        move.l  (a2),(a1)               ;(i+1) nach (i) kopieren
        move.l  d3,(a2)                 ;d3 nach (i+1) kopieren
        bra.s   bs_k_loop

bs_fin
        movem.l (sp)+,a2-a4/d2-d5      ;register wiederherstellen
        rts


*======================================================================
* Unterprogramm decin
* konvertiert einen ASCII-String in eine Zahl von typ Long.
*
* Parameter: A0 -> Adresse des Strings
*
* Rückgabe: Long-Wert in D1
*======================================================================
decin
        move.l  a0,a1                   ;Kopier von a0 in a1
        sub.l   d1,d1                   ;d1 löschen
        cmp.b   #'-',(a0)               ;erstes Byte von lineBuf ist '-'?
        bne.s   decin2                  ;nein, dann anfangen
    
        add     #1,a0                   ;sonst auf nächses Zeichen zeigen

decin2    
        bsr     decinloop               ;test subroutin
        cmp.b   #'-',(a1)               ;erstes Byte des Strings ist '-'?
        bne.s   decin3                  ;nein, dann fertig
    
        neg.l   d1                      ;ja => negative Zahl: d1 negieren
    
decin3    
        rts     
            
decinloop   
        bsr     digitin                 ;convert digit
        cmp     #10,d0                  ;test,if valid
        bcc     decinok                 ;no,then done
        mulu    #10,d1                  ;shift result
        add     d0,d1                   ;insert nibble
        bra     decinloop               ;and continue
    
decinok   
        rts                             ;end of conversion
    
digitin                                 ;converting the nibble from (A0)
    
        clr.l   d0                      ;erase D0
        move.b  (a0)+,d0                ;get digit,increment A0
        sub     #'0',d0                 ;subtract $30
        rts

*======================================================================
* Unterprogramm PrintDosErr
* Gibt eine AmigaDOS-Fehlermeldung auf der Konsole aus.
*
* Parameter: keiner; aber vorher muss mit pea die Adresse eines
*   Unterprogrammes auf den Stack gelegt werden, bei dem das
*   Programm nach PrintDosErr fortgesetzt wird.
*======================================================================

PrintDosErr
        move.l  _DOSBase,a6             ;dos.library verwenden

        jsr     _LVOIoErr(a6);          ;Fehlercode holen
        move.l  d0,d1                   ;und nach d1
        moveq   #0,d2                   ;kein eigener Text voranstellt
        jmp     _LVOPrintFault(a6)      ;Fehlertext ausgeben

*======================================================================
* Unterprogramm PrintStatistics
* Gibt Informationen über die Datei und die benötigten Zeiten für
* das Zählen, Laden und Sorieren aus.
*======================================================================
    
PrintStatistics   
        move.l  _DOSBase,a6             ;dos.library verwenden
    
        lea     strFmt1(pc),a0
        move.l  a0,d1                   ;FormatString
        move.l  0,d2                    ;Argument: /
        jsr     _LVOVPrintf(a6)         ;VPrintf (dos.library)
    
        lea     strFmt2(pc),a0
        move.l  a0,d1                   ;FormatString für Aufruf nach d1
        lea     argArray(pc),a0
        move.l  a0,d2                   ;Argument: Arg[0] -> Dateiname
        jsr     _LVOVPrintf(a6)         ;VPrintf (dos.library)
    
    
        lea     strFmt3(pc),a0
        move.l  a0,d1                   ;FormatString
        lea     lineCount(pc),a0        
        move.l  a0,d2                   ;Argument: Adresse von lineCount
        jsr     _LVOVPrintf(a6)         ;VPrintf (dos.library)
    
    
        lea     strFmt4(pc),a0
        move.l  a0,d1                   ;FormatString1
        lea     timeCounting(pc),a0
        move.l  a0,d2                   ;Argument: timeCounting
        jsr     _LVOVPrintf(a6)         ;VPrintf (dos.library)
    
    
        lea     strFmt5(pc),a0
        move.l  a0,d1                   ;FormatString
        lea     timeLoading(pc),a0
        move.l  a0,d2                   ;Argument: timeLoading
        jsr     _LVOVPrintf(a6)         ;VPrintf (dos.library)
    
    
        lea     strFmt6(pc),a0
        move.l  a0,d1                   ;FormatString
        lea     timeSorting(pc),a0
        move.l  a0,d2                   ;Argument: timeSorting
        jsr     _LVOVPrintf(a6)         ;VPrintf (dos.library)

        rts


*======================================================================
* Datenbereich
*======================================================================

dosname         DOSNAME
timername       TIMERNAME
mathname        dc.b            "mathieeesingbas.library",0
                even  
  
_DOSBase        ds.l            1
_MATHBase       ds.l            1
_TimerBase      ds.l            1
  
bDosReturnErr   ds.b            1       ;bool-Variable. Beschreibt, ob
                                        ;Fehlercode am Programmende.
  
MessagePort     ds.l            1
  
TimerIO         ds.l            1
  
  
EClockStart     ds.l            EV_SIZE ;EClock-Wert zum Start-Zeitpunkt
EClockStop      ds.l            EV_SIZE ;EClock-Wert zum Start-Zeitpunkt
  
clocksPerSecond ds.l            1       ;Anzahl timer-clocks pro Sekunde
  
lineCount       ds.l            1       ;Anzahl Zeilen in Datei
timeCounting    ds.l            1       ;Zeit für das Zählen der Zeilen
timeLoading     ds.l            1       ;Zeit für das Laden der Datei
timeSorting     ds.l            1       ;Zeit für das Sortieren Zeilen
  
allLinesBuf     ds.l            1       ;Puffer für alle (Longword)-Werte der Datei
                even  
  
lineBuf         ds.b            LINE_LEN ;Zeilenpuffer zum Einlesen
                even  
  
inputHandle     ds.l            1
                even  
  
argTemplate     dc.b            'F=FILE/A,P=PRINT/S',0 ;Template für ReadArgs
                even  
  
argArray        ds.l            2
                even  
  
strFmt1         dc.b            '==========================================',10,0
strFmt2         dc.b            'Sorted file ''%s'' with ',0
strFmt3         dc.b            '%ld lines.',10,0
strFmt4         dc.b            '  Time of line counting = %ld ms',10,0
strFmt5         dc.b            '  Time of line loading = %ld ms',10,0
strFmt6         dc.b            '  Time for bubble sort = %ld ms',10,0
strFmtNumber    dc.b            '%ld',10,0
strPrint        dc.b            'PRINT argument was given:-)',10,0
strNoPrint      dc.b            'PRINT argument was *not* given.',10,0
  
strErrAllocVec  dc.b          'Can''t allocate memory.',10,0
strErrOpMathLib dc.b          'Can''t open mathieeesingbas.library.',10,0

                END
