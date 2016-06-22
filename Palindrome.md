Palindrome detection

ACL2
(defun reverse-split-at-r (xs i ys)
  (if (zp i)
      (mv xs ys)
      (reverse-split-at-r (rest xs) (1- i)
                          (cons (first xs) ys))))
 
(defun reverse-split-at (xs i)
  (reverse-split-at-r xs i nil))
 
(defun is-palindrome (str)
  (let* ((lngth (length str))
         (idx (floor lngth 2)))
    (mv-let (xs ys)
            (reverse-split-at (coerce str 'list) idx)
            (if (= (mod lngth 2) 1)
                (equal (rest xs) ys)
                (equal xs ys)))))
ActionScript
The following function handles non-ASCII characters properly, since charAt() returns a single Unicode character. 
function isPalindrome(str:String):Boolean
{
        for(var first:uint = 0, second:uint = str.length - 1; first < second; first++, second--)
                if(str.charAt(first) != str.charAt(second)) return false;
        return true;
}
Ada
function Palindrome (Text : String) return Boolean is
begin
   for Offset in 0..Text'Length / 2 - 1 loop
      if Text (Text'First + Offset) /= Text (Text'Last - Offset) then
         return False;
      end if;
   end loop;
   return True;
end Palindrome;
ALGOL 68
Translation of: C
Works with: ALGOL 68 version Standard - no extensions to language used
Works with: ALGOL 68G version Any - tested with release mk15-0.8b.fc9.i386
Works with: ELLA ALGOL 68 version Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - except for the FORMAT and printf in test
# Iterative #
PROC palindrome = (STRING s)BOOL:(
   FOR i TO UPB s OVER 2 DO
     IF s[i] /= s[UPB s-i+1] THEN GO TO return false FI
   OD;Power
   else: TRUE EXIT
   return false: FALSE
);
 
# Recursive #
PROC palindrome r = (STRING s)BOOL:
   IF LWB s >= UPB s THEN TRUE
   ELIF s[LWB s] /= s[UPB s] THEN FALSE
   ELSE palindrome r(s[LWB s+1:UPB s-1])
   FI
;
 
# Test #
main:
(
   STRING t = "ingirumimusnocteetconsumimurigni";
   FORMAT template = $"sequence """g""" "b("is","isnt")" a palindrome"l$;
 
   printf((template, t, palindrome(t)));
   printf((template, t, palindrome r(t)))
)
Output:
sequence "ingirumimusnocteetconsumimurigni" is a palindrome
sequence "ingirumimusnocteetconsumimurigni" is a palindrome
Applesoft BASIC
100 DATA"MY DOG HAS FLEAS"
110 DATA"MADAM, I'M ADAM."
120 DATA"1 ON 1"
130 DATA"IN GIRUM IMUS NOCTE ET CONSUMIMUR IGNI"
140 DATA"A man, a plan, a canal: Panama!"
150 DATA"KAYAK"
160 DATA"REDDER"
170 DATA"H"
180 DATA""
 
200 FOR L1 = 1 TO 9
210    READ W$ : GOSUB 300" IS PALINDROME?
220    PRINT CHR$(34); W$; CHR$(34); " IS ";
230    IF NOT PALINDROME THEN PRINT "NOT ";
240    PRINT "A PALINDROME"
250 NEXT
260 END
 
300 REMIS PALINDROME?
310 PA = 1
320 L = LEN(W$)
330 IF L = 0 THEN RETURN
340 FOR L0 = 1 TO L / 2 + .5
350     PA = MID$(W$, L0, 1) = MID$(W$, L - L0 + 1, 1)
360     IF PALINDROME THEN NEXT L0
370 RETURN
AutoHotkey
Reversing the string: 
IsPalindrome(Str){ACL2
(defun reverse-split-at-r (xs i ys)
  (if (zp i)
      (mv xs ys)
      (reverse-split-at-r (rest xs) (1- i)
                          (cons (first xs) ys))))
 
(defun reverse-split-at (xs i)
  (reverse-split-at-r xs i nil))
 
(defun is-palindrome (str)
  (let* ((lngth (length str))
         (idx (floor lngth 2)))
    (mv-let (xs ys)
            (reverse-split-at (coerce str 'list) idx)
            (if (= (mod lngth 2) 1)
                (equal (rest xs) ys)
                (equal xs ys)))))
ActionScript
The following function handles non-ASCII characters properly, since charAt() returns a single Unicode character. 
function isPalindrome(str:String):Boolean
{
        for(var first:uint = 0, second:uint = str.length - 1; first < second; first++, second--)
                if(str.charAt(first) != str.charAt(second)) return false;
        return true;
}
Ada
function Palindrome (Text : String) return Boolean is
begin
   for Offset in 0..Text'Length / 2 - 1 loop
      if Text (Text'First + Offset) /= Text (Text'Last - Offset) then
         return False;
      end if;
   end loop;
   return True;
end Palindrome;
ALGOL 68
Translation of: C
Works with: ALGOL 68 version Standard - no extensions to language used
Works with: ALGOL 68G version Any - tested with release mk15-0.8b.fc9.i386
Works with: ELLA ALGOL 68 version Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - except for the FORMAT and printf in test
# Iterative #
PROC palindrome = (STRING s)BOOL:(
   FOR i TO UPB s OVER 2 DO
     IF s[i] /= s[UPB s-i+1] THEN GO TO return false FI
   OD;Power
   else: TRUE EXIT
   return false: FALSE
);
 
# Recursive #
PROC palindrome r = (STRING s)BOOL:
   IF LWB s >= UPB s THEN TRUE
   ELIF s[LWB s] /= s[UPB s] THEN FALSE
   ELSE palindrome r(s[LWB s+1:UPB s-1])
   FI
;
 
# Test #
main:
(
   STRING t = "ingirumimusnocteetconsumimurigni";
   FORMAT template = $"sequence """g""" "b("is","isnt")" a palindrome"l$;
 
   printf((template, t, palindrome(t)));
   printf((template, t, palindrome r(t)))
)
Output:
sequence "ingirumimusnocteetconsumimurigni" is a palindrome
sequence "ingirumimusnocteetconsumimurigni" is a palindrome
Applesoft BASIC
100 DATA"MY DOG HAS FLEAS"
110 DATA"MADAM, I'M ADAM."
120 DATA"1 ON 1"
130 DATA"IN GIRUM IMUS NOCTE ET CONSUMIMUR IGNI"
140 DATA"A man, a plan, a canal: Panama!"
150 DATA"KAYAK"
160 DATA"REDDER"
170 DATA"H"
180 DATA""
 
200 FOR L1 = 1 TO 9
210    READ W$ : GOSUB 300" IS PALINDROME?
220    PRINT CHR$(34); W$; CHR$(34); " IS ";
230    IF NOT PALINDROME THEN PRINT "NOT ";
240    PRINT "A PALINDROME"
250 NEXT
260 END
 
300 REMIS PALINDROME?
310 PA = 1
320 L = LEN(W$)
330 IF L = 0 THEN RETURN
340 FOR L0 = 1 TO L / 2 + .5
350     PA = MID$(W$, L0, 1) = MID$(W$, L - L0 + 1, 1)
360     IF PALINDROME THEN NEXT L0
370 RETURN

        Loop, Parse, Str
                ReversedStr := A_LoopField . ReversedStr
        return, (ReversedStr == Str)?"Exact":(RegExReplace(ReversedStr,"\W")=RegExReplace(Str,"\W"))?"Inexact":"False"
}
AutoIt
;== AutoIt Version: 3.3.8.1
 
Global $aString[7] = [ _
"In girum imus nocte, et consumimur igni", _  ; inexact palindrome
"Madam, I'm Adam.", _                         ; inexact palindrome
"salàlas", _                                  ; exact palindrome
"radar", _                                    ; exact palindrome
"Lagerregal", _                               ; exact palindrome
"Ein Neger mit Gazelle zagt im Regen nie.", _ ; inexact palindrome
"something wrong"]                            ; no palindrome
Global $sSpace42 = "                                          "
 
For $i = 0 To 6
        If _IsPalindrome($aString[$i]) Then
                ConsoleWrite('"' & $aString[$i] & '"' & StringLeft($sSpace42, 42-StringLen($aString[$i])) & 'is an exact palindrome.' & @LF)
        Else
                If _IsPalindrome( StringRegExpReplace($aString[$i], '\W', '') ) Then
                        ConsoleWrite('"' & $aString[$i] & '"' & StringLeft($sSpace42, 42-StringLen($aString[$i])) & 'is an  inexact palindrome.' & @LF)
                Else
                        ConsoleWrite('"' & $aString[$i] & '"' & StringLeft($sSpace42, 42-StringLen($aString[$i])) & 'is not a palindrome.' & @LF)
                EndIf
        EndIf
Next
 
Func _IsPalindrome($_string)
        Local $iLen = StringLen($_string)
        For $i = 1  To Int($iLen/2)
                If StringMid($_string, $i, 1) <> StringMid($_string, $iLen-($i-1), 1) Then Return False
        Next
        Return True
EndFunc
 
Output:
 
"In girum imus nocte, et consumimur igni"   is an inexact palindrome.
"Madam, I'm Adam."                          is an inexact palindrome.
"salàlas"                                   is an exact palindrome.
"radar"                                     is an exact palindrome.
"Lagerregal"                                is an exact palindrome.
"Ein Neger mit Gazelle zagt im Regen nie."  is an inexact palindrome.
"something wrong"                           is not a palindrome.
 
--BugFix (talk) 14:26, 13 November 2013 (UTC) 
AWK
Non-recursive 
See Reversing a string. 
function is_palindro(s)
{
  if ( s == reverse(s) ) return 1
  return 0
}
Recursive 
function is_palindro_r(s)
{
  if ( length(s) < 2 ) return 1
  if ( substr(s, 1, 1) != substr(s, length(s), 1) ) return 0
  return is_palindro_r(substr(s, 2, length(s)-2))
}
Testing 
BEGIN {
  pal = "ingirumimusnocteetconsumimurigni"
  print is_palindro(pal)
  print is_palindro_r(pal)
}
BASIC
This example is incorrect. The stripping of spaces and case conversion should be outside the palindrome detection. Please fix the code and remove this message. 

Works with: QBasic
DECLARE FUNCTION isPalindrome% (what AS STRING)
 
DATA "My dog has fleas", "Madam, I'm Adam.", "1 on 1", "In girum imus nocte et consumimur igni"
 
DIM L1 AS INTEGER, w AS STRING
FOR L1 = 1 TO 4
    READ w
    IF isPalindrome(w) THEN
        PRINT CHR$(34); w; CHR$(34); " is a palindrome"
    ELSE
        PRINT CHR$(34); w; CHR$(34); " is not a palindrome"
    END IF
NEXT
 
FUNCTION isPalindrome% (what AS STRING)
    DIM whatcopy AS STRING, chk AS STRING, tmp AS STRING * 1, L0 AS INTEGER
 
    FOR L0 = 1 TO LEN(what)
        tmp = UCASE$(MID$(what, L0, 1))
        SELECT CASE tmp
            CASE "A" TO "Z"
                whatcopy = whatcopy + tmp
                chk = tmp + chk
            CASE "0" TO "9"
                PRINT "Numbers are cheating! ("; CHR$(34); what; CHR$(34); ")"
                isPalindrome = 0
                EXIT FUNCTION
        END SELECT
    NEXT
 
    isPalindrome = ((whatcopy) = chk)
END FUNCTION
Output:
"My dog has fleas" is not a palindrome
"Madam, I'm Adam." is a palindrome
Numbers are cheating! ("1 on 1")
"1 on 1" is not a palindrome
"In girum imus nocte et consumimur igni" is a palindrome
BBC BASIC
      test$ = "A man, a plan, a canal: Panama!"
      PRINT """" test$ """" ;
      IF FNpalindrome(FNletters(test$)) THEN
        PRINT " is a palindrome"
      ELSE
        PRINT " is not a palindrome"
      ENDIF
      END
 
      DEF FNpalindrome(A$) = (A$ = FNreverse(A$))
 
      DEF FNreverse(A$)
      LOCAL B$, P%
      FOR P% = LEN(A$) TO 1 STEP -1
        B$ += MID$(A$,P%,1)
      NEXT
      = B$
 
      DEF FNletters(A$)
      LOCAL B$, C%, P%
      FOR P% = 1 TO LEN(A$)
        C% = ASC(MID$(A$,P%))
        IF C% > 64 AND C% < 91 OR C% > 96 AND C% < 123 THEN
          B$ += CHR$(C% AND &5F)
        ENDIF
      NEXT
      = B$
Output:
"A man, a plan, a canal: Panama!" is a palindrome
Batch File
@echo off
setlocal enabledelayedexpansion
set /p string=Your string :
set count=0
:loop
        if "!%string%:~%count%,1!" neq "" (
                set reverse=!%string%:~%count%,1!!reverse!
                set /a count+=1
                goto loop
        )
set palindrome=isn't
if "%string%"=="%reverse%" set palindrome=is
echo %string% %palindrome% a palindrome.
pause
exit
Or, recursive (and without setlocal enabledelayedexpansion) (compatible with ReactOS cmd.exe) 
@echo off
set /p testString=Your string (all same case please) : 
call :isPalindrome result %testString: =%
if %result%==1 echo %testString% is a palindrome
if %result%==0 echo %testString% isn't a palindrome
pause
goto :eof
 
:isPalindrome
        set %1=0
        set string=%2
        if "%string:~2,1%"=="" (
                set %1=1
                goto :eof
        )
        if "%string:~0,1%"=="%string:~-1%" (
                call :isPalindrome %1 %string:~1,-1%
        )
        goto :eof
Befunge
Works with: CCBI version 2.1
The following code reads a line from stdin and prints "True" if it is a palindrome, or False" otherwise. 
v_$0:8p>:#v_:18p08g1-08p >:08g`!v
~->p5p ^  0v1p80-1g80vj!-g5g80g5_0'ev
:a^80+1:g8<>8g1+:18pv>0"eslaF">:#,_@ 
[[relet]]-2010------>003-x   -^"Tru"<
Works with: Befunge version 93
To check a string, replace "dennis sinned" with your own string. 
Note that this has some limits.: 
There must be a quotation mark immediately after the string, and then nothing but spaces for the rest of that line. 
The v at the end of that same line must remain immediately above the 2. (Very important.) The closing quotation mark can be against the v, but can't replace it.
The potential palindrome can be no longer than 76 characters (which beats the previous version's 11), and everything (spaces, punctuation, capitalization, etc.) is considered part of the palindrome. (Best to just use lower case letters and nothing else.)
v>    "emordnilap a toN",,,,,,,,,,,,,,,,@,,,,,,,,,,,,,,,"Is a palindrome"     <
2^ < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < <
4    ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v
8  ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v # ^_v
*^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v   ^_v
+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
>"dennis sinned"                                                               v
 "                                                                             2
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" 0
> ^- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 9
   _^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^  p
    v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^ # v_^
      v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^   v_^
 ^< < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < <
 >09g8p09g1+09pv
 |:            <                                                               <
^<
Bracmat
( ( palindrome
  =   a
    .     @(!arg:(%?a&utf$!a) ?arg !a)
        & palindrome$!arg
      | utf$!arg
  )
& ( desep
  =   x
    .     @(!arg:?x (" "|"-"|",") ?arg)
        & !x desep$!arg
      | !arg
  )
&     "In girum imus nocte et consumimur igni"
      "Я иду с мечем, судия"
      "The quick brown fox"
      "tregða, gón, reiði - er nóg að gert"
      "人人為我,我為人人"
      "가련하시다 사장집 아들딸들아 집장사 다시 하련가"
  : ?candidates
&   whl
  ' ( !candidates:%?candidate ?candidates
    &   out
      $ ( !candidate
          is
          (   palindrome$(low$(str$(desep$!candidate)))
            & indeed
          | not
          )
          a
          palindrome
        )
    )
&
);
Output: 
In girum imus nocte et consumimur igni is indeed a palindrome
Я иду с мечем, судия is indeed a palindrome
The quick brown fox is not a palindrome
tregða, gón, reiði - er nóg að gert is indeed a palindrome
人人為我,我為人人 is indeed a palindrome
  가련하시다 사장집 아들딸들아 집장사 다시 하련가
  is
  indeed
  a
  palindrome
Burlesque
 
zz{ri}f[^^<-==
 
C
Non-recursive 
This function compares the first char with the last, the second with the one previous the last, and so on. The first different pair it finds, return 0 (false); if all the pairs were equal, then return 1 (true). You only need to go up to (the length) / 2 because the second half just re-checks the same stuff as the first half; and if the length is odd, the middle doesn't need to be checked (so it's okay to do integer division by 2, which rounds down). 
#include <string.h>
 
int palindrome(const char *s)
{
   int i,l;
   l = strlen(s);
   for(i=0; i<l/2; i++)
   {
     if ( s[i] != s[l-i-1] ) return 0; 
   }
   return 1;
}
More idiomatic version: 
int palindrome(const char *s)
{
   const char *t; /* t is a pointer that traverses backwards from the end */
   for (t = s; *t != '\0'; t++) ; t--; /* set t to point to last character */
   while (s < t)
   {
     if ( *s++ != *t-- ) return 0; 
   }
   return 1;
}
Recursive 
A single char is surely a palindrome; a string is a palindrome if first and last char are the same and the remaining string (the string starting from the second char and ending to the char preceding the last one) is itself a palindrome. 
int palindrome_r(const char *s, int b, int e)
{
   if ( (e - 1) <= b ) return 1;
   if ( s[b] != s[e-1] ) return 0;
   return palindrome_r(s, b+1, e-1);
}
Testing 
#include <stdio.h>
#include <string.h>
/* testing */
int main()
{
   const char *t = "ingirumimusnocteetconsumimurigni";
   const char *template = "sequence \"%s\" is%s palindrome\n";
   int l = strlen(t);
 
   printf(template,
          t, palindrome(t) ? "" : "n't");
   printf(template,
          t, palindrome_r(t, 0, l) ? "" : "n't");
   return 0;
}
C++
The C solutions also work in C++, but C++ allows a simpler one: 
#include <string>
#include <algorithm>
 
bool is_palindrome(std::string const& s)
{
  return std::equal(s.begin(), s.end(), s.rbegin());
}
Or, checking half is sufficient (on odd-length strings, this will ignore the middle element): 
#include <string>
#include <algorithm>
 
bool is_palindrome(std::string const& s)
{
  return std::equal(s.begin(), s.begin()+s.length()/2, s.rbegin());
}
C#
Non-recursive 
using System;
 
class Program
{
    static string Reverse(string value)
    {
        char[] chars = value.ToCharArray();
        Array.Reverse(chars);
        return new string(chars);
    }
 
    static bool IsPalindrome(string value)
    {
        return value == Reverse(value);
    }
 
    static void Main(string[] args)
    {
        Console.WriteLine(IsPalindrome("ingirumimusnocteetconsumimurigni"));
    }
}
Using LINQ operators 
using System;
using System.Linq;
 
class Program
{
        static bool IsPalindrome(string text)
        {
                return text == new String(text.Reverse().ToArray());
        }
 
        static void Main(string[] args)
        {
                Console.WriteLine(IsPalindrome("ingirumimusnocteetconsumimurigni"));
        }
}
 
Clojure
(defn palindrome? [s]
  (= s (apply str (reverse s))))
Recursive 
(defn palindrome? [s]
  (loop [i 0
         j (dec (. s length))]
    (cond (>= i j) true
          (= (get s i) (get s j))
            (recur (inc i) (dec j))
          :else false)))
Test 
user=> (palindrome? "amanaplanacanalpanama")
true
user=> (palindrome? "Test 1, 2, 3")
false
CoffeeScript
 
    String::isPalindrome = ->
        for i in [0...@length / 2] when @[i] isnt @[@length - (i + 1)]
            return no
        yes
 
    String::stripped = -> @toLowerCase().replace /\W/gi, ''
 
    console.log "'#{ str }' : #{ str.stripped().isPalindrome() }" for str in [
        'In girum imus nocte et consumimur igni'
        'A man, a plan, a canal: Panama!'
        'There is no spoon.'
    ]
 
Output:
   'In girum imus nocte et consumimur igni' : true
   'A man, a plan, a canal: Panama!' : true
   'There is no spoon.' : false
Common Lisp
(defun palindrome-p (s)
  (string= s (reverse s)))
Component Pascal
BlackBox Component Builder 
 
MODULE BbtPalindrome;
IMPORT StdLog;
 
PROCEDURE ReverseStr(str: ARRAY OF CHAR): POINTER TO ARRAY OF CHAR;
VAR
        top,middle,i: INTEGER;
        c: CHAR;
        rStr: POINTER TO ARRAY OF CHAR;
BEGIN
        NEW(rStr,LEN(str$) + 1);
        top := LEN(str$) - 1; middle := (top - 1) DIV 2;
        FOR i := 0 TO middle DO
                rStr[i] := str[top - i];
                rStr[top - i] := str[i];
        END;
        IF ODD(LEN(str$)) THEN rStr[middle + 1] := str[middle + 1] END;
        RETURN rStr;
END ReverseStr;
 
PROCEDURE IsPalindrome(str: ARRAY OF CHAR): BOOLEAN;
BEGIN
        RETURN str = ReverseStr(str)$;
END IsPalindrome;
 
PROCEDURE Do*;
VAR
        x: CHAR;
BEGIN
        StdLog.String("'salalas' is palindrome?:> ");
        StdLog.Bool(IsPalindrome("salalas"));StdLog.Ln;
        StdLog.String("'madamimadam' is palindrome?:> ");
        StdLog.Bool(IsPalindrome("madamimadam"));StdLog.Ln;
        StdLog.String("'abcbda' is palindrome?:> ");
        StdLog.Bool(IsPalindrome("abcbda"));StdLog.Ln;
END Do;
END BbtPalindrome.
 
Execute: ^Q BbtPalindrome.Do
Output:
'salalas' is palindrome?:>  $TRUE
'madamimadam' is palindrome?:>  $TRUE
'abcbda' is palindrome?:>  $FALSE
Delphi
uses
  SysUtils, StrUtils;
 
function IsPalindrome(const aSrcString: string): Boolean;
begin
  Result := SameText(aSrcString, ReverseString(aSrcString));
end;
D
High-level 32-bit Unicode Version
import std.traits, std.algorithm;
 
bool isPalindrome1(C)(in C[] s) pure /*nothrow*/
if (isSomeChar!C) {
    auto s2 = s.dup;
    s2.reverse(); // works on Unicode too, not nothrow.
    return s == s2;
}
 
void main() {
    alias pali = isPalindrome1;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    assert(pali("salÃ las"));
}
Mid-level 32-bit Unicode Version
import std.traits;
 
bool isPalindrome2(C)(in C[] s) pure if (isSomeChar!C) {
    dchar[] dstr;
    foreach (dchar c; s) // not nothrow
        dstr ~= c;
 
    for (int i; i < dstr.length / 2; i++)
        if (dstr[i] != dstr[$ - i - 1])
            return false;
    return true;
}
 
void main() {
    alias isPalindrome2 pali;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    assert(pali("salÃ las"));
}
Low-level 32-bit Unicode Version
import std.stdio, core.exception, std.traits;
 
// assume alloca() to be pure for this program
extern(C) pure nothrow void* alloca(in size_t size);
 
bool isPalindrome3(C)(in C[] s) pure if (isSomeChar!C) {
    auto p = cast(dchar*)alloca(s.length * 4);
    if (p == null)
        // no fallback heap allocation used
        throw new OutOfMemoryError();
    dchar[] dstr = p[0 .. s.length];
 
    // use std.utf.stride for an even lower level version
    int i = 0;
    foreach (dchar c; s) { // not nothrow
        dstr[i] = c;
        i++;
    }
    dstr = dstr[0 .. i];
 
    foreach (j; 0 .. dstr.length / 2)
        if (dstr[j] != dstr[$ - j - 1])
            return false;
    return true;
}
 
void main() {
    alias isPalindrome3 pali;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    assert(pali("salÃ las"));
}
Low-level ASCII Version
bool isPalindrome4(in string str) pure nothrow {
    if (str.length == 0) return true;
    immutable(char)* s = str.ptr;
    immutable(char)* t = &(str[$ - 1]);
    while (s < t)
        if (*s++ != *t--) // ugly
            return false;
    return true;
}
 
void main() {
    alias isPalindrome4 pali;
    assert(pali(""));
    assert(pali("z"));
    assert(pali("aha"));
    assert(pali("sees"));
    assert(!pali("oofoe"));
    assert(pali("deified"));
    assert(!pali("Deified"));
    assert(pali("amanaplanacanalpanama"));
    assert(pali("ingirumimusnocteetconsumimurigni"));
    //assert(pali("salÃ las"));
}
Dart
 
bool isPalindrome(String s){  
  for(int i = 0; i < s.length/2;i++){
    if(s[i] != s[(s.length-1) -i])
      return false;        
  }  
  return true;  
}
 
Déjà Vu
palindrome?:
        local :seq chars
        local :len-seq -- len seq
 
        for i range 0 / len-seq 2:
                if /= seq! i seq! - len-seq i:
                        return false
        true
 
!. palindrome? "ingirumimusnocteetconsumimurigni"
!. palindrome? "nope"
Output:
true
false
E
It is only necessarily to scan the first half of the string, upper(0, upper.size() // 2), and compare each character to the corresponding character from the other end, upper[last - i]. 
The for loop syntax is for key pattern => value pattern in collection { ... }, ? imposes an additional boolean condition on a pattern (it may be read “such that”), and if the pattern does not match in a for loop then the iteration is skipped, so false is returned only if upper[last - i] != c. 
def isPalindrome(string :String) {
  def upper := string.toUpperCase()
  def last := upper.size() - 1
  for i => c ? (upper[last - i] != c) in upper(0, upper.size() // 2) { 
    return false
  }
  return true
}
EchoLisp
 
;; returns #t or #f
(define (palindrome? string)
(equal? (string->list string) (reverse (string->list string))))
 
;; to strip spaces, use the following
;;(define (palindrome? string)
;;(let ((string (string-replace string "/\ /" "" "g")))
;;(equal? (string->list string) (reverse (string->list string)))))
 
Eiffel
 
        is_palindrome (a_string: STRING): BOOLEAN
                        -- Is `a_string' a palindrome?
                require
                        string_attached: a_string /= Void
                local
                        l_index, l_count: INTEGER
                do
                        from
                                Result := True
                                l_index := 1
                                l_count := a_string.count
                        until
                                l_index >= l_count - l_index + 1 or not Result
                        loop
                                Result := (Result and a_string [l_index] = a_string [l_count - l_index + 1])
                                l_index := l_index + 1
                        end
                end
 
Ela
open list string
 
isPalindrome xs = xs == reverse xs
isPalindrome <| toList "ingirumimusnocteetconsumimurigni"
 
Function reverse is taken from list module and is defined as: 
reverse = foldl (flip (::)) (nil xs)
 
foldl f z (x::xs) = foldl f (f z x) xs
foldl _ z []      = z
 
Elixir
 
defmodule PalindromeDetection do
  def is_palindrome(str), do: str == String.reverse(str)
end
 
Note: Because of Elixir's strong Unicode support, this even supports graphemes: 
iex(1)> PalindromeDetection.is_palindrome("salàlas")
true
iex(2)> PalindromeDetection.is_palindrome("as⃝df̅")
false
iex(3)> PalindromeDetection.is_palindrome("as⃝df̅f̅ds⃝a")
true
Elm
import String exposing (reverse, length) 
import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple exposing (start)
 
-- The following function (copied from Haskell) satisfies the 
-- rosettacode task description.
is_palindrome x = x == reverse x
 
-- The remainder of the code demonstrates the use of the function 
-- in a complete Elm program.
main = start { model = "", view = view, update = update }
 
update newStr oldStr = newStr
 
view : Address String -> String -> Html
view address candidate =
  div []
    ([ input
        [ placeholder "Enter a string to check."
        , value candidate
        , on "input" targetValue (Signal.message address)
        , myStyle
        ]
        []
     ] ++ 
     [ let testResult = 
             is_palindrome candidate
 
           statement = 
             if testResult then "PALINDROME!!" else "not a palindrome"
 
       in div [ myStyle] [text statement]
     ])
 
myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "20px")
    , ("padding", "5px 0 0 5px")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]
Link to live demo: http://dc25.github.io/palindromeDetectionElm/ 
Erlang
 
-module( palindrome ).
 
-export( [is_palindrome/1, task/0] ).
 
is_palindrome( String ) -> String =:= lists:reverse(String).
 
task() ->
        display( "abcba" ),
        display( "abcdef" ),
        Latin = "In girum imus nocte et consumimur igni",
        No_spaces_same_case = lists:append( string:tokens(string:to_lower(Latin), " ") ),
        display( Latin, No_spaces_same_case ).
 
 
 
display( String ) -> io:fwrite( "Is ~p a palindrom? ~p~n", [String, is_palindrome(String)] ).
 
display( String1, String2 ) -> io:fwrite( "Is ~p a palindrom? ~p~n", [String1, is_palindrome(String2)] ).
 
Output:
22> palindrome:task().
Is "abcba" a palindrom? true
Is "abcdef" a palindrom? false
Is "In girum imus nocte et consumimur igni" a Latin palindrom? true
Euphoria
function isPalindrome(sequence s)
    for i = 1 to length(s)/2 do
        if s[i] != s[$-i+1] then
            return 0
        end if
    end for
    return 1
end function
F#
let isPalindrome (s: string) =
   let arr = s.ToCharArray()
   arr = Array.rev arr
Examples: 
isPalindrome "abcba"
val it : bool = true
isPalindrome ("In girum imus nocte et consumimur igni".Replace(" ", "").ToLower());;
val it : bool = true
isPalindrome "abcdef"
val it : bool = false
Factor
USING: kernel sequences ;
: palindrome? ( str -- ? ) dup reverse = ;
Fantom
 
class Palindrome
{
  // Function to test if given string is a palindrome
  public static Bool isPalindrome (Str str) 
  {
    str == str.reverse
  }
 
  // Give it a test run
  public static Void main ()
  {
    echo (isPalindrome(""))
    echo (isPalindrome("a"))
    echo (isPalindrome("aa"))
    echo (isPalindrome("aba"))
    echo (isPalindrome("abb"))
    echo (isPalindrome("salàlas"))
    echo (isPalindrome("In girum imus nocte et consumimur igni".lower.replace(" ","")))
  }
}
 
FBSL
#APPTYPE CONSOLE
 
FUNCTION stripNonAlpha(BYVAL s AS STRING) AS STRING
        DIM sTemp AS STRING = ""
        DIM c AS STRING
        FOR DIM i = 1 TO LEN(s)
                c = MID(s, i, 1)
                IF INSTR("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c, 0, 1) THEN
                        sTemp = stemp & c
                END IF
        NEXT
        RETURN sTemp
END FUNCTION
 
FUNCTION IsPalindrome(BYVAL s AS STRING) AS INTEGER
        FOR DIM i = 1 TO STRLEN(s) \ 2 ' only check half of the string, as scanning from both ends
                IF s{i} <> s{STRLEN - (i - 1)} THEN RETURN FALSE 'comparison is not case sensitive
        NEXT
 
        RETURN TRUE
END FUNCTION
 
PRINT IsPalindrome(stripNonAlpha("A Toyota"))
PRINT IsPalindrome(stripNonAlpha("Madam, I'm Adam"))
PRINT IsPalindrome(stripNonAlpha("the rain in Spain falls mainly on the rooftops"))
 
PAUSE
 
Output:
 1 
 1
 0
Forth
: first   over c@ ;
: last    >r 2dup + 1- c@ r> swap ;
: palindrome? ( c-addr u -- f )
  begin
    dup 1 <=      if 2drop true  exit then
    first last <> if 2drop false exit then
    1 /string 1-
  again ;
 
FIRST and LAST are once-off words that could be beheaded immediately afterwards. The version taking advantage of Tail Call Optimization or a properly tail-recursive variant of RECURSE (easily added to any Forth) is very similar. The horizontal formatting highlights the parallel code - and potential factor; a library of many string tests like this could have ?SUCCESS and ?FAIL . 
Fortran
Works with: Fortran version 90 and later
program palindro
 
  implicit none
 
  character(len=*), parameter :: p = "ingirumimusnocteetconsumimurigni"
 
  print *, is_palindro_r(p)
  print *, is_palindro_r("anothertest")
  print *, is_palindro2(p)
  print *, is_palindro2("test")
  print *, is_palindro(p)
  print *, is_palindro("last test")
 
contains
Non-recursive 
! non-recursive
function is_palindro(t)
  logical :: is_palindro
  character(len=*), intent(in) :: t
 
  integer :: i, l
 
  l = len(t)
  is_palindro = .false.
  do i=1, l/2
     if ( t(i:i) /= t(l-i+1:l-i+1) ) return
  end do
  is_palindro = .true.
end function is_palindro
 
! non-recursive 2
function is_palindro2(t) result(isp)
  logical :: isp
  character(len=*), intent(in) :: t
 
  character(len=len(t)) :: s
  integer :: i
 
  forall(i=1:len(t)) s(len(t)-i+1:len(t)-i+1) = t(i:i)
  isp = ( s == t )
end function is_palindro2
Recursive 
  recursive function is_palindro_r (t) result (isp)
 
    implicit none
    character (*), intent (in) :: t
    logical :: isp
 
    isp = len (t) == 0 .or. t (: 1) == t (len (t) :) .and. is_palindro_r (t (2 : len (t) - 1))
 
  end function is_palindro_r
end program palindro
FreeBASIC
' version 20-06-2015
' compile with: fbc -s console "filename".bas
 
#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf
 
Function reverse(norm As String) As Integer
 
    Dim As String rev
    Dim As Integer i, l = Len(norm) -1
 
    rev = norm
    For i = 0 To l
        rev[l-i] = norm[i]
    Next
 
    If norm = rev Then
        Return TRUE
    Else
        Return FALSE
    End If
 
End Function
 
Function cleanup(in As String, action As String = "") As String
    ' action = "" do nothing, [l|L] = convert to lowercase,
    ' [s|S] = strip spaces,  [p|P] = strip punctuation.
    If action = "" Then Return in
 
    Dim As Integer i, p_, s_
    Dim As String ch
 
    action = LCase(action)
    For i = 1 To Len(action)
        ch = Mid(action, i, 1)
        If ch = "l" Then in = LCase(in)
        If ch = "p" Then
            p_ = 1
        ElseIf ch = "s" Then
            s_ = 1
        End If
    Next
 
    If p_ = 0 And s_ = 0 Then Return in
 
    Dim As String unwanted, clean
 
    If s_ = 1 Then unwanted = " "
    If p_ = 1 Then unwanted = unwanted + "`~!@#$%^&*()-=_+[]{}\|;:',.<>/?"
 
    For i = 1 To Len(in)
        ch = Mid(in, i, 1)
        If InStr(unwanted, ch) = 0 Then clean = clean + ch
    Next
 
    Return clean
 
End Function
 
' ------=< MAIN >=------
 
Dim As String test = "In girum imus nocte et consumimur igni"
'IIf ( cond, true, false ), true and false must be of the same type (num, string, UDT)
Print
Print "                 reverse(test) = "; IIf(reverse(test) = FALSE, "FALSE", "TRUE")
Print "  reverse(cleanup(test,""l"")) = "; IIf(reverse(cleanup(test,"l")) = FALSE, "FALSE", "TRUE")
Print " reverse(cleanup(test,""ls"")) = "; IIf(reverse(cleanup(test,"ls")) = FALSE, "FALSE", "TRUE")
Print "reverse(cleanup(test,""PLS"")) = "; IIf(reverse(cleanup(test,"PLS")) = FALSE, "FALSE", "TRUE")
 
' empty keyboard buffer
While InKey <> "" : Var _key_ = InKey : Wend
Print : Print : Print "Hit any key to end program"
Sleep
End
Output:
               reverse(test) = FALSE
  reverse(cleanup(test,"l")) = FALSE
 reverse(cleanup(test,"ls")) = TRUE
reverse(cleanup(test,"PLS")) = TRUE
Frink
This version will even work with upper-plane Unicode characters. Many implementations will not work correctly with upper-plane Unicode characters because they are represented as Unicode "surrogate pairs" which are represented as two characters in a UTF-16 stream. 
isPalindrome[x] := x == reverse[x]
 
Test in Frink with upper-plane Unicode: 
isPalindrome["x\u{1f638}x"]
true 
GAP
ZapGremlins := function(s)
  local upper, lower, c, i, n, t;
  upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  lower := "abcdefghijklmnopqrstuvwxyz";
  t := [ ];
  i := 1;
  for c in s do
    n := Position(upper, c);
    if n <> fail then
      t[i] := lower[n];
      i := i + 1;
    else
      n := Position(lower, c);
      if n <> fail then
        t[i] := c;
        i := i + 1;
      fi;
    fi;
  od;
  return t;
end;
 
IsPalindrome := function(s)
  local t;
  t := ZapGremlins(s);
  return t = Reversed(t);
end;
GML
 
//Setting a var from an argument passed to the script
var str;
str = argument0
//Takes out all spaces/anything that is not a letter or a number and turns uppercase letters to lowercase
str = string_lettersdigits(string_lower(string_replace(str,' ','')));
var inv;
inv = '';
//for loop that reverses the sequence
var i;
for (i = 0; i < string_length(str); i += 1;)
    {
    inv += string_copy(str,string_length(str)-i,1);
    }
//returns true if the sequence is a palindrome else returns false
return (str == inv);
 
Palindrome detection using a Downward For-Loop 
 
 
//Remove everything except for letters and digits and convert the string to lowercase. source is what will be compared to str.
var str = string_lower(string_lettersdigits(string_replace(argument0," ",""))), source = "";
 
//Loop through and store each character of str in source.
for (var i = string_length(str); i > 0; i--) {
    source += string_char_at(str,i);
}
 
//Return if it is a palindrome.
return source == str;
 
Go
package pal
 
func IsPal(s string) bool {
    mid := len(s) / 2
    last := len(s) - 1
    for i := 0; i < mid; i++ {
        if s[i] != s[last-i] {
            return false
        }
    }
    return true
}
Groovy
Trivial
Solution: 
def isPalindrome = { String s ->
    s == s?.reverse()
}
Test program: 
println isPalindrome("")
println isPalindrome("a")
println isPalindrome("abcdefgfedcba")
println isPalindrome("abcdeffedcba")
println isPalindrome("abcedfgfedcb")
Output:
true
true
true
true
false
This solution assumes nulls are palindromes. 
Non-recursive
Solution: 
def isPalindrome = { String s ->
    def n = s.size()
    n < 2 || s[0..<n/2] == s[-1..(-n/2)]
}
Test program and output are the same. This solution does not handle nulls. 
Recursive
Solution follows the C palindrome_r recursive solution: 
def isPalindrome
isPalindrome = { String s ->
    def n = s.size()
    n < 2 || (s[0] == s[n-1] && isPalindrome(s[1..<(n-1)]))
}
Test program and output are the same. This solution does not handle nulls. 
Haskell
Non-recursive 
A string is a palindrome if reversing it we obtain the same string. 
is_palindrome x = x == reverse x
Recursive 
See the C palindrome_r code for an explanation of the concept used in this solution. 
is_palindrome_r x | length x <= 1 = True
                  | head x == last x = is_palindrome_r . tail. init $ x
                  | otherwise = False
HicEst
This example is incorrect. The stripping of spaces and case conversion should be outside the palindrome detection. Please fix the code and remove this message. 

   result = Palindrome( "In girum imus nocte et consumimur igni" ) ! returns 1
END
 
FUNCTION Palindrome(string)
   CHARACTER string, CopyOfString
 
   L = LEN(string)
   ALLOCATE(CopyOfString, L)
   CopyOfString = string
   EDIT(Text=CopyOfString, UpperCase=L)
   L = L - EDIT(Text=CopyOfString, End, Left=' ', Delete, DO=L) ! EDIT returns number of deleted spaces
 
   DO i = 1, L/2
     Palindrome = CopyOfString(i) == CopyOfString(L - i + 1)
     IF( Palindrome == 0 ) RETURN
   ENDDO
END
Icon and Unicon
procedure main(arglist)
every writes(s := !arglist) do write( if palindrome(s) then " is " else " is not", " a palindrome.")
end
The following simple procedure uses the built-in reverse. Reverse creates a transient string which will get garbage collected. 
procedure palindrome(s)  #: return s if s is a palindrome
return s == reverse(s)
end
Library: Icon Programming Library
Note: The IPL procedure strings contains a palindrome tester called ispal that uses reverse and is equivalent to the version of palindrome above. 
This version uses positive and negative sub-scripting and works not only on strings but lists of strings, such as ["ab","ab"] or ["ab","x"] the first list would pass the test but the second wouldn't. 
procedure palindrome(x)  #: return x if s is x palindrome
local i
every if x[i := 1 to (*x+ 1)/2] ~== x[-i] then fail
return x
end
Ioke
Text isPalindrome? = method(self chars == self chars reverse)
J
Non-recursive 
Reverse and match method 
isPalin0=: -: |.
Example usage 
   isPalin0 'ABBA'
1
   isPalin0 -.&' ' tolower 'In girum imus nocte et consumimur igni'
1
Recursive 
Tacit and explicit verbs: 
isPalin1=: 0:`($:@(}.@}:))@.({.={:)`1:@.(1>:#)
 
isPalin2=: monad define
 if. 1>:#y do. 1 return. end.
 if. ({.={:)y do. isPalin2 }.}:y else. 0 end.
)
Note that while these recursive verbs are bulkier and more complicated, they are also several thousand times more inefficient than isPalin0. 
   foo=: foo,|.foo=:2000$a.
   ts=:6!:2,7!:2  NB. time and space required to execute sentence
   ts 'isPalin0 foo'
2.73778e_5 5184
   ts 'isPalin1 foo'
0.0306667 6.0368e6
   ts 'isPalin2 foo'
0.104391 1.37965e7
   'isPalin1 foo' %&ts 'isPalin0 foo'
1599.09 1164.23
   'isPalin2 foo' %&ts 'isPalin0 foo'
3967.53 2627.04
Java
Non-Recursive 
public static boolean pali(String testMe){
        StringBuilder sb = new StringBuilder(testMe);
        return testMe.equals(sb.reverse().toString());
}
Recursive (this version does not work correctly with upper-plane Unicode) 
public static boolean rPali(String testMe){
        if(testMe.length()<=1){
                return true;
        }
        if(!(testMe.charAt(0)+"").equals(testMe.charAt(testMe.length()-1)+"")){
                return false;
        }
        return rPali(testMe.substring(1, testMe.length()-1));
}
Recursive using indexes (this version does not work correctly with upper-plane Unicode) 
public static boolean rPali(String testMe){
        int strLen = testMe.length();
        return rPaliHelp(testMe, strLen-1, strLen/2, 0);
}
 
public static boolean rPaliHelp(String testMe, int strLen, int testLen, int index){
        if(index > testLen){
                return true;
        }
        if(testMe.charAt(index) != testMe.charAt(strLen-index)){
                return false;
        }
        return rPaliHelp(testMe, strLen, testLen, index + 1);
}
 
Regular Expression (source) 
public static boolean pali(String testMe){
        return testMe.matches("|(?:(.)(?<=(?=^.*?(\\1\\2?)$).*))+(?<=(?=^\\2$).*)");
}
JavaScript
function isPalindrome(str) {
  return str === str.split("").reverse().join("");
}
 
console.log(isPalindrome("ingirumimusnocteetconsumimurigni"));
ES6 implementation 
var isPal = str => str === str.split("").reverse().join("");
jq
def palindrome: explode as $in | ($in|reverse) == $in;
Example: 
"salàlas" | palindrome
Output:
true
Julia
palindrome(s) = s == reverse(s)
Non-Recursive 
 
function palindrome(s)
    len = length(s)
    for i = 1:(len/2)
        if(s[len-i+1]!=s[i])
            return false
        end
    end
    return true
end
 
Recursive 
 
function palindrome(s)
    len = length(s)
    if(len==0 || len==1)
        return true
    end
    if(s[1] == s[len])
        return palindrome(SubString(s,2,len-1))
    end
    return false
end
k
is_palindrome:{x~|x}
LabVIEW
This image is a VI Snippet, an executable image of LabVIEW code. The LabVIEW version is shown on the top-right hand corner. You can download it, then drag-and-drop it onto the LabVIEW block diagram from a file browser, and it will appear as runnable, editable code.
 

Lasso
define ispalindrome(text::string) => {
 
        local(_text = string(#text)) // need to make copy to get rid of reference issues
 
        #_text -> replace(regexp(`(?:$|\W)+`), -ignorecase)
 
        local(reversed = string(#_text))
        #reversed -> reverse
 
        return #_text == #reversed
}
 
ispalindrome('Tätatät') // works with high ascii
ispalindrome('Hello World')
 
ispalindrome('A man, a plan, a canoe, pasta, heros, rajahs, a coloratura, maps, snipe, percale, macaroni, a gag, a banana bag, a tan, a tag, a banana bag again (or a camel), a crepe, pins, Spam, a rut, a Rolo, cash, a jar, sore hats, a peon, a canal – Panama!')
Output:
true
false
true
Liberty BASIC
print isPalindrome("In girum imus nocte et consumimur igni")
print isPalindrome(removePunctuation$("In girum imus nocte et consumimur igni", "S"))
print isPalindrome(removePunctuation$("In girum imus nocte et consumimur igni", "SC"))
 
function isPalindrome(string$)
    isPalindrome = 1
    for i = 1 to int(len(string$)/2)
        if mid$(string$, i, 1) <> mid$(string$, len(string$)-i+1, 1) then isPalindrome = 0 : exit function
    next i
end function
 
function removePunctuation$(string$, remove$)
    'P = remove puctuation.  S = remove spaces   C = remove case
    If instr(upper$(remove$), "C") then string$ = lower$(string$)
    If instr(upper$(remove$), "P") then removeCharacters$ = ",.!'()-&*?<>:;~[]{}"
    If instr(upper$(remove$), "S") then removeCharacters$ = removeCharacters$;" "
 
    for i = 1 to len(string$)
        if instr(removeCharacters$, mid$(string$, i, 1)) then string$ = left$(string$, i-1);right$(string$, len(string$)-i) : i = i - 1
    next i
    removePunctuation$ = string$
end function
Output:
0
0
1
LiveCode
This implementation defaults to exact match, but has an optional parameter to do inexact.
function palindrome txt exact
    if exact is empty or exact is not false then 
        set caseSensitive to true  --default is false
    else
        replace space with empty in txt
        put lower(txt) into txt
    end if
    return txt is reverse(txt) 
end palindrome
 
function reverse str
    repeat with i = the length of str down to 1
        put byte i of str after revstr
    end repeat
    return revstr
end reverse
Logo
to palindrome? :w
  output equal? :w reverse :w
end
Lua
function ispalindrome(s) return s == string.reverse(s) end
M4
Non-recursive This uses the invert from Reversing a string. 
define(`palindrorev',`ifelse(`$1',invert(`$1'),`yes',`no')')dnl
palindrorev(`ingirumimusnocteetconsumimurigni')
palindrorev(`this is not palindrome')
Recursive 
define(`striptwo',`substr(`$1',1,eval(len(`$1')-2))')dnl
define(`cmplast',`ifelse(`striptwo(`$1')',,`yes',dnl
substr(`$1',0,1),substr(`$1',eval(len(`$1')-1),1),`yes',`no')')dnl
define(`palindro',`dnl
ifelse(eval(len(`$1')<1),1,`yes',cmplast(`$1'),`yes',`palindro(striptwo(`$1'))',`no')')dnl
palindro(`ingirumimusnocteetconsumimurigni')
palindro(`this is not palindrome')

Maple
This uses functions from Maple's built-in StringTools package. 
 
with(StringTools):
 
IsPalindrome("ingirumimusnocteetconsumimurigni");
 
IsPalindrome("In girum imus nocte et consumimur igni");
 
IsPalindrome(LowerCase(DeleteSpace("In girum imus nocte et consumimur igni")));
 
Output:
                                    true

                                    false

                                    true
Mathematica
Custom functions: 
Non-recursive 
PalindromeQ[i_String] := StringReverse[i] == i
Test numbers: 
PalindromeQ[i_Integer] := Reverse[IntegerDigits[i]] == IntegerDigits[i]; 
Examples:
PalindromeQ["TNT"]
PalindromeRecQ["TNT"]
PalindromeQ["test"]
PalindromeRecQ["test"]
PalindromeQ["deified"] 
PalindromeRecQ["deified"] 
PalindromeQ["salàlas"]  
PalindromeRecQ["salàlas"]   
PalindromeQ["ingirumimusnocteetconsumimurigni"]
PalindromeRecQ["ingirumimusnocteetconsumimurigni"]
Note that the code block doesn't correctly show the à in salàlas. 
Output:
True
True
False
False
True
True
True
True
True
True
MATLAB
function trueFalse = isPalindrome(string)
 
    trueFalse = all(string == fliplr(string)); %See if flipping the string produces the original string
 
    if not(trueFalse) %If not a palindrome
        string = lower(string); %Lower case everything
        trueFalse = all(string == fliplr(string)); %Test again
    end
 
    if not(trueFalse) %If still not a palindrome
        string(isspace(string)) = []; %Strip all space characters out
        trueFalse = all(string == fliplr(string)); %Test one last time
    end
 
end
Sample Usage:
>> isPalindrome('In girum imus nocte et consumimur igni')
 
ans =
 
     1
 
Maxima
palindromep(s) := block([t], t: sremove(" ", sdowncase(s)), sequal(t, sreverse(t)))$
 
palindromep("Sator arepo tenet opera rotas");  /* true */
MAXScript
Non-recursive 
fn isPalindrome s =
(
    local reversed = ""
    for i in s.count to 1 by -1 do reversed += s[i]
    return reversed == s
)
Recursive 
fn isPalindrome_r s =
(
    if s.count <= 1 then
    (
        true
    )
    else
    (
        if s[1] != s[s.count] then
        (
            return false
        )
        isPalindrome_r (substring s 2 (s.count-2))
    )
)
Testing 
local p = "ingirumimusnocteetconsumimurigni"
format ("'%' is a palindrome? %\n") p (isPalindrome p)
format ("'%' is a palindrome? %\n") p (isPalindrome_r p)
Mirah
def reverse(s:string) 
    StringBuilder.new(s).reverse.toString()
end
 
def palindrome?(s:string) 
    s.equals(reverse(s))
end
 
puts palindrome?("anna")        # ==> true
puts palindrome?("Erik")        # ==> false
puts palindrome?("palindroom-moordnilap") # ==> true
puts nil                        # ==> null
ML
mLite
fun to_locase s = implode ` map (c_downcase) ` explode s
 
fun only_alpha s = implode ` filter (fn x = c_alphabetic x) ` explode s
 
fun is_palin
        ( h1 :: t1, h2 :: t2, n = 0 )                  = true
|       ( h1 :: t1, h2 :: t2, n ) where ( h1 eql h2 )  = is_palin( t1, t2, n - 1)
|       ( h1 :: t1, h2 :: t2, n )                      = false
|       (str s) =
                let
                        val es = explode ` to_locase ` only_alpha s;
                        val res = rev es;
                        val k = (len es) div 2
                in
                        is_palin (es, res, k)
                end
 
fun test_is_palin s =
        (print "\""; print s; print "\" is a palindrome: "; print ` is_palin s; println "")
 
fun test (f, arg, res, ok, notok) = if (f arg eql res) then ("'" @ arg @ "' " @ ok) else ("'" @ arg @ "' " @ notok)
 
;
 
println ` test (is_palin, "In girum imus nocte, et consumimur igni", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "Madam, I'm Adam.", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "salàlas", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "radar", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "Lagerregal", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "Ein Neger mit Gazelle zagt im Regen nie.", true, "is a palindrome", "is NOT a palindrome");
println ` test (is_palin, "something wrong", true, "is a palindrome", "is NOT a palindrome");
Output: 
'In girum imus nocte, et consumimur igni' is a palindrome
'Madam, I'm Adam.' is a palindrome
'salàlas' is a palindrome
'radar' is a palindrome
'Lagerregal' is a palindrome
'Ein Neger mit Gazelle zagt im Regen nie.' is a palindrome
'something wrong' is NOT a palindrome
Standard ML
 
fun palindrome s =
  let val cs = explode s in
    cs = rev cs
  end
 
MMIX
argc     IS $0
argv     IS $1
 
         LOC Data_Segment
DataSeg  GREG @
 
          LOC @+1000
ItsPalStr IS @-Data_Segment
          BYTE "It's palindrome",10,0
          LOC @+(8-@)&7
NoPalStr  IS  @-Data_Segment
          BYTE "It is not palindrome",10,0
 
         LOC #100
         GREG @
% input: $255 points to where the string to be checked is
% returns $255 0 if not palindrome, not zero otherwise
% trashs: $0,$1,$2,$3
% return address $4
DetectPalindrome LOC @
         ADDU $1,$255,0      % $1 = $255
2H       LDB  $0,$1,0        % get byte at $1
         BZ   $0,1F          % if zero, end (length)
         INCL $1,1           % $1++
         JMP  2B             % loop
1H       SUBU $1,$1,1        % ptr last char of string
         ADDU $0,DataSeg,0   % $0 to data seg.
3H       CMP  $3,$1,$255     % is $0 == $255?
         BZ   $3,4F          % then jump
         LDB  $3,$1,0        % otherwise get the byte
         STB  $3,$0,0        % and copy it
         INCL $0,1           % $0++
         SUB  $1,$1,1        % $1--
         JMP  3B
4H       LDB  $3,$1,0
         STB  $3,$0,0        % copy the last byte
% now let us compare reversed string and straight string
         XOR  $0,$0,$0       % index
         ADDU $1,DataSeg,0
6H       LDB  $2,$1,$0       % pick char from rev str
         LDB  $3,$255,$0     % pick char from straight str
         BZ   $3,PaliOk      % finished as palindrome
         CMP  $2,$2,$3       % == ?
         BNZ  $2,5F          % if not, exit
         INCL $0,1           % $0++
         JMP  6B
5H       XOR  $255,$255,$255
         GO   $4,$4,0        % return false
PaliOk   NEG  $255,0,1       
         GO   $4,$4,0        % return true
% The Main for testing the function
% run from the command line
% $ mmix ./palindrome.mmo ingirumimusnocteetconsumimurigni
Main     CMP  argc,argc,2    % argc > 2?
         BN   argc,3F        % no -> not enough arg
         ADDU $1,$1,8        % argv+1
         LDOU $255,$1,0      % argv[1]
         GO   $4,DetectPalindrome
         BZ   $255,2F        % if not palindrome, jmp
         SETL $0,ItsPalStr   % pal string
         ADDU $255,DataSeg,$0
         JMP  1F
2H       SETL $0,NoPalStr    % no pal string
         ADDU $255,DataSeg,$0
1H       TRAP 0,Fputs,StdOut % print
3H       XOR  $255,$255,$255
         TRAP 0,Halt,0       % exit(0)
Modula-3
MODULE Palindrome;
 
IMPORT Text;
 
PROCEDURE isPalindrome(string: TEXT): BOOLEAN =
  VAR len := Text.Length(string);
  BEGIN
    FOR i := 0 TO len DIV 2 - 1 DO
      IF Text.GetChar(string, i) # Text.GetChar(string, (len - i - 1)) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END isPalindrome;
END Palindrome.
Nemerle
using System;
using System.Console;
using Nemerle.Utility.NString; //contains methods Explode() and Implode() which convert string -> list[char] and back
 
module Palindrome
{
    IsPalindrome( text : string) : bool
    {
        Implode(Explode(text).Reverse()) == text;
    }
 
    Main() : void
    {
        WriteLine("radar is a palindrome: {0}", IsPalindrome("radar"));
    }
}
And a function to remove spaces and punctuation and convert to lowercase 
Clean( text : string ) : string
{
    def sepchars = Explode(",.;:-?!()' ");
    Concat( "", Split(text, sepchars)).ToLower()
}
NetRexx
Translation of: REXX
 
y='In girum imus nocte et consumimur igni'
 
-- translation: We walk around in the night and
-- we are burnt by the fire (of love)
say
say 'string = 'y
say
 
pal=isPal(y)
 
if pal==0 then say "The string isn't palindromic."
          else say 'The string is palindromic.'
 
method isPal(x) static
  x=x.upper().space(0)          /* removes all blanks (spaces)          */
                                /*   and translate to uppercase.        */
  return x==x.reverse()         /* returns  1  if exactly equal         */
 
NewLISP
Works likewise for strings and for lists 
 
(define (palindrome? s)
    (setq r s)
    (reverse r) ; Reverse is destructive.
    (= s r))
 
;; Make ‘reverse’ non-destructive and avoid a global variable
(define (palindrome? s)
    (= s (reverse (copy s))))
 
Nim
proc reverse(s): string =
  result = newString(s.len)
  for i,c in s:
    result[s.high - i] = c
 
proc isPalindrome(s): bool =
  s == reverse(s)
 
echo isPalindrome("FoobooF")
Objeck
 
bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      IsPalindrome("aasa")->PrintLine();
      IsPalindrome("acbca")->PrintLine();
      IsPalindrome("xx")->PrintLine();
    }
 
    function : native : IsPalindrome(s : String) ~ Bool {
      l := s->Size();
      for(i := 0; i < l / 2; i += 1;) {
        if(s->Get(i) <> s->Get(l - i - 1)) {
          return false;
        };
      };
 
      return true;
    }
  }
}
 
OCaml
let is_palindrome s =
    let l = String.length s in
    let rec comp n =
        n = 0 || (s.[l-n] = s.[n-1] && comp (n-1)) in
    comp (l / 2)
and here a function to remove the white spaces in the string: 
let rem_space str =
  let len = String.length str in
  let res = String.create len in
  let rec aux i j =
    if i >= len
    then (String.sub res 0 j)
    else match str.[i] with
    | ' ' | '\n' | '\t' | '\r' ->
        aux (i+1) (j)
    | _ ->
        res.[j] <- str.[i];
        aux (i+1) (j+1)
  in
  aux 0 0
and to make the test case insensitive, just use the function String.lowercase. 

Oforth
String method: isPalindrome { self reverse self == }
Octave
Recursive 
function v = palindro_r(s)
  if ( length(s) == 1 )
    v = true;
    return;
  elseif ( length(s) == 2 )
    v = s(1) == s(2);
    return;
  endif
  if ( s(1) == s(length(s)) )
    v = palindro_r(s(2:length(s)-1));
  else
    v = false;
  endif
endfunction
Non-recursive 
function v = palindro(s)
  v = all( (s == s(length(s):-1:1)) == 1);
endfunction
Testing 
palindro_r("ingirumimusnocteetconsumimurigni")
palindro("satorarepotenetoperarotas")
Oz
fun {IsPalindrome S}
  {Reverse S} == S
end
PARI/GP
ispal(s)={
  s=Vec(s);
  for(i=1,#v\2,
    if(v[i]!=v[#v-i+1],return(0))
  );
  1
};
A version for numbers: 
Works with: PARI/GP version 2.6.0 and above
ispal(s)={
  my(d=digits(n));
  for(i=1,#d\2,
    if(d[i]!=d[n+1=i],return(0))
  );
  1
};
Pascal
Works with: Free Pascal
program Palindro;
 
{ RECURSIVE }
function is_palindro_r(s : String) : Boolean;
begin
   if length(s) <= 1 then
      is_palindro_r := true
   else begin
      if s[1] = s[length(s)] then
         is_palindro_r := is_palindro_r(copy(s, 2, length(s)-2))
      else
         is_palindro_r := false
   end
end; { is_palindro_r }
 
{ NON RECURSIVE; see [[Reversing a string]] for "reverse" }
function is_palindro(s : String) : Boolean;
begin
   if s = reverse(s) then
      is_palindro := true
   else
      is_palindro := false
end;
procedure test_r(s : String; r : Boolean);
begin
   write('"', s, '" is ');
   if ( not r ) then
      write('not ');
   writeln('palindrome')
end;
 
var
   s1, s2 : String;
 
begin
   s1 := 'ingirumimusnocteetconsumimurigni';
   s2 := 'in girum imus nocte';
   test_r(s1, is_palindro_r(s1));
   test_r(s2, is_palindro_r(s2));
   test_r(s1, is_palindro(s1));
   test_r(s2, is_palindro(s2))
end.
Perl
There is more than one way to do this. 
palindrome uses the built-in function reverse(). 
palindrome_c uses iteration; it is a translation of the C solution. 
palindrome_r uses recursion. 
palindrome_e uses a recursive regular expression.
All of these functions take a parameter, or default to $_ if there is no parameter. None of these functions ignore case or strip characters; if you want do that, you can use ($s = lc $s) =~ s/[\W_]//g before you call these functions. 
# Palindrome.pm
package Palindrome;
 
use strict;
use warnings;
 
use Exporter 'import';
our @EXPORT = qw(palindrome palindrome_c palindrome_r palindrome_e);
 
sub palindrome
{
    my $s = (@_ ? shift : $_);
    return $s eq reverse $s;
}
 
sub palindrome_c
{
    my $s = (@_ ? shift : $_);
    for my $i (0 .. length($s) >> 1)
    {
        return 0 unless substr($s, $i, 1) eq substr($s, -1 - $i, 1);
    }
    return 1;
}
 
sub palindrome_r
{
    my $s = (@_ ? shift : $_);
    if (length $s <= 1) { return 1; }
    elsif (substr($s, 0, 1) ne substr($s, -1, 1)) { return 0; }
    else { return palindrome_r(substr($s, 1, -1)); }
}
 
sub palindrome_e
{
    (@_ ? shift : $_) =~ /^(.?|(.)(?1)\2)$/ + 0
}
This example shows how to use the functions: 
# pbench.pl
use strict;
use warnings;
 
use Benchmark qw(cmpthese);
use Palindrome;
 
printf("%d, %d, %d, %d: %s\n",
       palindrome, palindrome_c, palindrome_r, palindrome_e, $_)
for
    qw/a aa ab abba aBbA abca abba1 1abba
    ingirumimusnocteetconsumimurigni/,
    'ab cc ba', 'ab ccb a';
 
printf "\n";
 
my $latin = "ingirumimusnocteetconsumimurigni";
cmpthese(100_000, {
    palindrome => sub { palindrome $latin },
    palindrome_c => sub { palindrome_c $latin },
    palindrome_r => sub { palindrome_r $latin },
    palindrome_e => sub { palindrome_e $latin },
});
Output:
on a machine running Perl 5.10.1 on amd64-openbsd: 
$ perl pbench.pl
1, 1, 1, 1: a
1, 1, 1, 1: aa
0, 0, 0, 0: ab
1, 1, 1, 1: abba
0, 0, 0, 0: aBbA
0, 0, 0, 0: abca
0, 0, 0, 0: abba1
0, 0, 0, 0: 1abba
1, 1, 1, 1: ingirumimusnocteetconsumimurigni
1, 1, 1, 1: ab cc ba
0, 0, 0, 0: ab ccb a

            (warning: too few iterations for a reliable count)
                  Rate palindrome_r palindrome_e palindrome_c   palindrome
palindrome_r   51020/s           --         -50%         -70%         -97%
palindrome_e  102041/s         100%           --         -41%         -94%
palindrome_c  172414/s         238%          69%           --         -90%
palindrome   1666667/s        3167%        1533%         867%           --
With this machine, palindrome() ran far faster than the alternatives (and too fast for a reliable count). The Perl regular expression engine recursed twice as fast as the Perl interpreter. 
Perl 6
subset Palindrom of Str where {
    .flip eq $_ given .comb(/\w+/).join.lc
}
 
my @tests = q:to/END/.lines;
    A man, a plan, a canal: Panama.
    My dog has fleas
    Madam, I'm Adam.
    1 on 1
    In girum imus nocte et consumimur igni
    END
 
for @tests { say $_ ~~ Palindrom, "\t", $_ }
Output:
True    A man, a plan, a canal: Panama.
False   My dog has fleas
True    Madam, I'm Adam.
False   1 on 1
True    In girum imus nocte et consumimur igni
Phix
function is_palindrome(sequence s)
    return s==reverse(s)
end function
 
?is_palindrome(lower(substitute("In girum imus nocte et consumimur igni"," ",""))) -- prints 1
PHP
<?php
function is_palindrome($string) {
  return $string == strrev($string);
}
?>
Regular expression-based solution (source) 
<?php
function is_palindrome($string) {
  return preg_match('/^(?:(.)(?=.*(\1(?(2)\2|))$))*.?\2?$/', $string);
}
?>
PicoLisp
(de palindrome? (S)
   (= (setq S (chop S)) (reverse S)) )
Output:
: (palindrome? "ingirumimusnocteetconsumimurigni")
-> T
Pike
int main(){
   if(pal("rotator")){
      write("palindrome!\n");
   }
   if(!pal("asdf")){
      write("asdf isn't a palindrome.\n");
   }
}
 
int pal(string input){
   if( reverse(input) == input ){
      return 1;
   } else {
      return 0;
   }
}
PL/I
To satisfy the revised specification (which contradicts the preceding explanation) the following trivially solves the problem in PL/I: 
is_palindrome = (text = reverse(text));
The following solution strips spaces: 
is_palindrome: procedure (text) returns (bit(1));
   declare text character (*) varying;
 
   text = remove_blanks(text);
   text = lowercase(text);
   return (text = reverse(text));
 
remove_blanks: procedure (text);
   declare text character (*) varying;
   declare (i, j) fixed binary (31);
   j = 0;
   do i = 1 to length(text);
      if substr(text, i, 1) = ' ' then
         do; j = j + 1; substr(text, j, 1) = substr(text, i, 1); end;
   end;
   return (substr(text, 1, j));
end remove_blanks;
end is_palindrome;
Potion
# The readable recursive version
palindrome_i = (s, b, e):
  if (e <= b): true.
  elsif (s ord(b) != s ord(e)): false.
  else: palindrome_i(s, b+1, e-1).
.
 
palindrome = (s):
  palindrome_i(s, 0, s length - 1).
 
palindrome(argv(1))
PowerBASIC
The output is identical to the QBasic version, above. 
FUNCTION isPalindrome (what AS STRING) AS LONG
    DIM whatcopy AS STRING, chk AS STRING, tmp AS STRING * 1, L0 AS LONG
 
    FOR L0 = 1 TO LEN(what)
        tmp = UCASE$(MID$(what, L0, 1))
        SELECT CASE tmp
            CASE "A" TO "Z"
                whatcopy = whatcopy & tmp
                chk = tmp & chk
            CASE "0" TO "9"
                MSGBOX "Numbers are cheating! (""" & what & """)"
                FUNCTION = 0
                EXIT FUNCTION
        END SELECT
    NEXT
 
    FUNCTION = ISTRUE((whatcopy) = chk)
END FUNCTION
 
 
FUNCTION PBMAIN () AS LONG
    DATA "My dog has fleas", "Madam, I'm Adam.", "1 on 1", "In girum imus nocte et consumimur igni"
    DIM L1 AS LONG, w AS STRING
    FOR L1 = 1 TO DATACOUNT
        w = READ$(L1)
        IF ISTRUE(isPalindrome(w)) THEN
            MSGBOX $DQ & w & """ is a palindrome"
        ELSE
            MSGBOX $DQ & w & """ is not a palindrome"
        END IF
    NEXT
END FUNCTION

PowerShell
 
Function Test-Palindrome( [String] $Text ){
    $CharArray = $Text.ToCharArray()
    [Array]::Reverse($CharArray)
    $Text.ToCharArray() -eq $CharArray
}
 
PowerShell (Regex Version)
This version is much faster because it does not manipulate arrays. 
 
function Test-Palindrome
{
  <#
    .SYNOPSIS
        Tests if a string is a palindrome.
    .DESCRIPTION
        Tests if a string is a true palindrome or, optionally, an inexact palindrome.
    .EXAMPLE
        Test-Palindrome -Text "racecar"
    .EXAMPLE
        Test-Palindrome -Text '"Deliver desserts," demanded Nemesis, "emended, named, stressed, reviled."' -Inexact
  #>
    [CmdletBinding()]
    [OutputType([bool])]
    Param
    (
        # The string to test for palindrominity.
        [Parameter(Mandatory=$true)]
        [string]
        $Text,
 
        # When specified, detects an inexact palindrome.
        [switch]
        $Inexact
    )
 
    if ($Inexact)
    {
        # Strip all punctuation and spaces
        $Text = [Regex]::Replace("$Text($7&","[^1-9a-zA-Z]","")
    }
 
    $Text -match "^(?'char'[a-z])+[a-z]?(?:\k'char'(?'-char'))+(?(char)(?!))$"
}
 
 
Test-Palindrome -Text 'radar'
 
Output:
True
 
Test-Palindrome -Text "In girum imus nocte et consumimur igni."
 
Output:
False
 
Test-Palindrome -Text "In girum imus nocte et consumimur igni." -Inexact
 
Output:
True
Prolog
Non-recursive 
From this tutorial. 
palindrome(Word) :- name(Word,List), reverse(List,List).
Recursive 
Works with: SWI Prolog
pali(Str) :- sub_string(Str, 0, 1, _, X), string_concat(Str2, X, Str), string_concat(X, Mid, Str2), pali(Mid).
pali(Str) :- string_length(Str, Len), Len < 2.
Changing string into atom makes the program run also on GNU Prolog. I.e. 
Works with: GNU Prolog
pali(Str) :- sub_atom(Str, 0, 1, _, X), atom_concat(Str2, X, Str), atom_concat(X, Mid, Str2), pali(Mid).
pali(Str) :- atom_length(Str, Len), Len < 2.
PureBasic
Works with: PureBasic version 4.41
Procedure IsPalindrome(StringToTest.s)
  If StringToTest=ReverseString(StringToTest)
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure
Python
Now that Python 2.7 and Python 3.4 are quite different, We should include the version IMHO. 
Non-recursive 
This one uses the reversing the string technique (to reverse a string Python can use the odd but right syntax string[::-1]) 
def is_palindrome(s):
  return s == s[::-1]
Recursive 
def is_palindrome_r(s):
  if len(s) <= 1:
    return True
  elif s[0] != s[-1]:
    return False
  else:
    return is_palindrome_r(s[1:-1])
Python has short-circuit evaluation of Boolean operations so a shorter and still easy to understand recursive function is 
def is_palindrome_r2(s):
  return not s or s[0] == s[-1] and is_palindrome_r2(s[1:-1])
Testing 
def test(f, good, bad):
  assert all(f(x) for x in good)
  assert not any(f(x) for x in bad)
  print '%s passed all %d tests' % (f.__name__, len(good)+len(bad))
 
pals = ('', 'a', 'aa', 'aba', 'abba')
notpals = ('aA', 'abA', 'abxBa', 'abxxBa')
for ispal in is_palindrome, is_palindrome_r, is_palindrome_r2:
  test(ispal, pals, notpals)
Palindrome Using Regular Expressions Python 2.7 
def p_loop():
  import re, string
  re1=""       # Beginning of Regex
  re2=""       # End of Regex
  pal=raw_input("Please Enter a word or phrase: ")
  pd = pal.replace(' ','')
  for c in string.punctuation:
     pd = pd.replace(c,"")
  if pal == "" :
    return -1
  c=len(pd)   # Count of chars.
  loops = (c+1)/2 
  for x in range(loops):
    re1 = re1 + "(\w)"
    if (c%2 == 1 and x == 0):
       continue 
    p = loops - x
    re2 = re2 + "\\" + str(p)
  regex= re1+re2+"$"   # regex is like "(\w)(\w)(\w)\2\1$"
  #print(regex)  # To test regex before re.search
  m = re.search(r'^'+regex,pd,re.IGNORECASE)
  if (m):
     print("\n   "+'"'+pal+'"')
     print("   is a Palindrome\n")
     return 1
  else:
     print("Nope!")
     return 0
R
Recursive 
Note that the recursive method will fail if the string length is too long. R will assume an infinite recursion if a recursion nests deeper than 5,000. Options may be set in the environment to increase this to 500,000. 
palindro <- function(p) {
  if ( nchar(p) == 1 ) {
    return(TRUE)
  } else if ( nchar(p) == 2 ) {
    return(substr(p,1,1) == substr(p,2,2))
  } else {
    if ( substr(p,1,1) == substr(p, nchar(p), nchar(p)) ) {
      return(palindro(substr(p, 2, nchar(p)-1)))
    } else {
      return(FALSE)
    }
  }
}
Iterative 
palindroi <- function(p) {
  for(i in 1:floor(nchar(p)/2) ) {
    r <- nchar(p) - i + 1
    if ( substr(p, i, i) != substr(p, r, r) ) return(FALSE) 
  }
  TRUE
}
Comparative 
This method is somewhat faster than the other two. 
Note that this method incorrectly regards an empty string as not a palindrome. Please leave this bug in the code, and take a look a the Testing_a_Function page. 
revstring <- function(stringtorev) {
   return(
      paste(
           strsplit(stringtorev,"")[[1]][nchar(stringtorev):1]
           ,collapse="")
           )
}
palindroc <- function(p) {return(revstring(p)==p)}
Output:
test <- "ingirumimusnocteetconsumimurigni"
tester <- paste(rep(test,38),collapse="")
> test <- "ingirumimusnocteetconsumimurigni"
> tester <- paste(rep(test,38),collapse="")
> system.time(palindro(tester))
   user  system elapsed 
   0.04    0.00    0.04 
> system.time(palindroi(tester))
   user  system elapsed 
   0.01    0.00    0.02 
> system.time(palindroc(tester))
   user  system elapsed 
      0       0       0 
Racket
 
(define (palindromb str)
  (let* ([lst (string->list (string-downcase str))]
         [slst (remove* '(#\space) lst)])
    (string=? (list->string (reverse slst)) (list->string slst))))
 
;;example output
 
> (palindromb "able was i ere i saw elba")
#t
> (palindromb "waht the hey")
#f
> (palindromb "In girum imus nocte et consumimur igni")
#t
> 
 
Rascal
The most simple solution: 
import String;
 
public bool palindrome(str text) =  toLowerCase(text) == reverse(text);
A solution that handles sentences with spaces and capitals: 
import String;
 
public bool palindrome(str text){
        text = replaceAll(toLowerCase(text), " ", "");
        return text == reverse(text);
}
 
Example: 
rascal>palindrome("In girum imus nocte et consumimur igni")
bool: true
REBOL
rebol [
    Title: "Palindrome Recognizer"
    Date: 2010-01-03
    Author: oofoe
    URL: http://rosettacode.org/wiki/Palindrome
]
 
; In order to compete with all the one-liners, the operation is
; compressed: parens force left hand side to evaluate first, where I
; copy the phrase, then uppercase it and assign it to 'p'. Now the
; right hand side is evaluated: p is copied, then reversed in place;
; the comparison is made and implicitely returned.
 
palindrome?: func [
        phrase [string!] "Potentially palindromatic prose."
        /local p
][(p: uppercase copy phrase) = reverse copy p]
 
; Teeny Tiny Test Suite
 
assert: func [code][print [either do code ["  ok"]["FAIL"]  mold code]]
 
print "Simple palindromes, with an exception for variety:"
repeat phrase ["z" "aha" "sees" "oofoe" "Deified"][
        assert compose [palindrome? (phrase)]]
 
print [crlf "According to the problem statement, these should fail:"]
assert [palindrome? "A man, a plan, a canal, Panama."] ; Punctuation not ignored.
assert [palindrome? "In girum imus nocte et consumimur igni"] ; Spaces not removed.
 
; I know we're doing palindromes, not alliteration, but who could resist...?
Output:
Simple palindromes, with an exception for variety:
  ok [palindrome? "z"]
  ok [palindrome? "aha"]
  ok [palindrome? "sees"]
FAIL [palindrome? "oofoe"]
  ok [palindrome? "Deified"]

According to the problem statement, these should fail:
FAIL [palindrome? "A man, a plan, a canal, Panama."]
FAIL [palindrome? "In girum imus nocte et consumimur igni"]
Retro
 
needs hash'
: palindrome? ( $-f ) dup ^hash'hash [ ^strings'reverse ^hash'hash ] dip = ;
 
"ingirumimusnocteetconsumimurigni" palindrome? putn
 
REXX
version 1
/*REXX pgm checks if phrase is palindromic; ignores the case of the letters.  */
parse arg y                            /*get (optional) phrase from the C.L.  */
if y=''  then y='In girum imus nocte et consumimur igni'    /*[↓] translation.*/
               /*We walk around in the night and we are burnt by the fire (of love).*/
say 'string = ' y
if isTpal(y)  then                   say 'The string is a true palindrome.'
              else if isPal(y)  then say 'The string is an inexact palindrome.'
                                else say "The string isn't palindromic."
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
isTpal:  return reverse(arg(1))==arg(1)
isPal:   return isTpal(translate(space(x,0)))
output 
string =  In girum imus nocte et consumimur igni
The string is an inexact palindrome.
Short version
Works with: ARexx
Works with: Regina
(3.8 and later, with options: AREXX_BIFS and AREXX_SEMANTICS) 
It should be noted that the   COMPRESS   function is not a Classic REXX BIF and isn't present in many REXXes. 
The   SPACE(string,0)   BIF can be used instead. 
It should also be noted that   UPPER   BIF is not present in some REXXes. 
Use the   PARSE UPPER   statement or   TRANSLATE()   BIF instead. 
 
/*Check whether a string is a palindrome */
parse pull string
select
        when palindrome(string) then say string 'is an exact palindrome.'
        when palindrome(compress(upper(string))) then say string 'is an inexact palindrome.'
        otherwise say string 'is not palindromic.'
        end
exit 0
 
palindrome: procedure
parse arg string
return string==reverse(string)
 
Output:
ABBA is an exact palindrome.
In girum imus nocte et consumimur igni is an inexact palindrome.
djdjdj is not palindromic.
Ring
 
aString = "radar"
bString = ""
for i=len(aString) to 1 step -1
    bString = bString + aString[i]
next
see aString
if aString = bString see " is a palindrome." + nl
else see " is not a palindrome" + nl ok
 
Ruby
Non-recursive 
def palindrome?(s)
  s == s.reverse
end
Recursive 
def r_palindrome?(s)
  if s.length <= 1
    true
  elsif s[0] != s[-1]
    false
  else
    r_palindrome?(s[1..-2])
  end
end
Testing Note that the recursive method is much slower -- using the 2151 character palindrome by Dan Hoey here, we have: 
str = "A man, a plan, a caret, [...2110 chars deleted...] a canal--Panama.".downcase.delete('^a-z')
puts palindrome?(str)    # => true
puts r_palindrome?(str)  # => true
 
require 'benchmark'
Benchmark.bm do |b|
  b.report('iterative') {10000.times {palindrome?(str)}}
  b.report('recursive') {10000.times {r_palindrome?(str)}}
end
Output:
true
true
               user     system      total        real
iterative  0.062000   0.000000   0.062000 (  0.055000)
recursive 16.516000   0.000000  16.516000 ( 16.562000)
Run BASIC
data "My dog has fleas", "Madam, I'm Adam.", "1 on 1", "In girum imus nocte et consumimur igni"
 
for i = 1 to 4
  read w$
  print w$;" is ";isPalindrome$(w$);" Palindrome"
next
 
FUNCTION isPalindrome$(str$)
for i = 1 to len(str$)
  a$ = upper$(mid$(str$,i,1))
   if (a$ >= "A" and a$ <= "Z") or (a$ >= "0" and a$ <= "9") then b$ = b$ + a$: c$ = a$ + c$
next i
if b$ <> c$ then isPalindrome$ = "not"
Output:
My dog has fleas is not Palindrome
Madam, I'm Adam. is  Palindrome
1 on 1 is not Palindrome
In girum imus nocte et consumimur igni is  Palindrome
Rust
 
fn is_palindrome(string: &str) -> bool {
    string.chars().zip(string.chars().rev()).all(|(x, y)| x == y)
}
 
macro_rules! test {
    ( $( $x:tt ),* ) => { $( println!("'{}': {}", $x, is_palindrome($x)); )* };
}
 
fn main() {
    test!("",
          "a",
          "ada",
          "adad",
          "ingirumimusnocteetconsumimurigni",
          "人人為我,我為人人",
          "Я иду с мечем, судия",
          "아들딸들아",
          "The quick brown fox");
}
 
Output:
'': true
'a': true
'ada': true
'adad': false
'ingirumimusnocteetconsumimurigni': true
'人人為我,我為人人': true
'Я иду с мечем, судия': false
'아들딸들아': true
'The quick brown fox': false
SAS
Description 
 
The macro "palindro" has two parameters: string and ignorewhitespace.
  string is the expression to be checked.
  ignorewhitespace, (Y/N), determines whether or not to ignore blanks and punctuation.
This macro was written in SAS 9.2.  If you use a version before SAS 9.1.3, 
the compress function options will not work.
 
Code 
 
%MACRO palindro(string, ignorewhitespace);
  DATA _NULL_;
    %IF %UPCASE(&ignorewhitespace)=Y %THEN %DO;
/* The arguments of COMPRESS (sp) ignore blanks and puncutation */
/* We take the string and record it in reverse order using the REVERSE function. */
      %LET rev=%SYSFUNC(REVERSE(%SYSFUNC(COMPRESS(&string,,sp)))); 
      %LET string=%SYSFUNC(COMPRESS(&string.,,sp));
    %END;
 
    %ELSE %DO;
      %LET rev=%SYSFUNC(REVERSE(&string));
    %END;
    /*%PUT rev=&rev.;*/
    /*%PUT string=&string.;*/
 
/* Here we determine if the string and its reverse are the same. */
    %IF %UPCASE(&string)=%UPCASE(&rev.) %THEN %DO;
      %PUT TRUE;
    %END;
    %ELSE %DO;
      %PUT FALSE; 
    %END;
  RUN;
%MEND;
 
Example macro call and output 
 
%palindro("a man, a plan, a canal: panama",y);
 
TRUE
 
NOTE: DATA statement used (Total process time):
      real time           0.00 seconds
      cpu time            0.00 seconds
 
%palindro("a man, a plan, a canal: panama",n);
 
FALSE
 
NOTE: DATA statement used (Total process time):
      real time           0.00 seconds
      cpu time            0.00 seconds
 
Scala
Library: Scala
Non-recursive, robustified
  def isPalindrome(s: String): Boolean = (s.size >= 2) && s == s.reverse
Bonus: Detect and account for odd space and punctuation
  def isPalindromeSentence(s: String): Boolean =
    (s.size >= 2) && {
      val p = s.replaceAll("[^\\p{L}]", "").toLowerCase
      p == p.reverse
    }
 
Recursive
import scala.annotation.tailrec
 
  def isPalindromeRec(s: String) = {
    @tailrec
    def inner(s: String): Boolean =
      (s.length <= 1) || (s.head == s.last) && inner(s.tail.init)
 
    (s.size >= 2) && inner(s)
  }
Testing 
  // Testing
  assert(!isPalindrome(""))
  assert(!isPalindrome("z"))
  assert(isPalindrome("amanaplanacanalpanama"))
  assert(!isPalindrome("Test 1,2,3"))
  assert(isPalindrome("1 2 1"))
  assert(!isPalindrome("A man a plan a canal Panama."))
 
  assert(!isPalindromeSentence(""))
  assert(!isPalindromeSentence("z"))
  assert(isPalindromeSentence("amanaplanacanalpanama"))
  assert(!isPalindromeSentence("Test 1,2,3"))
  assert(isPalindromeSentence("1 2 1"))
  assert(isPalindromeSentence("A man a plan a canal Panama."))
 
  assert(!isPalindromeRec(""))
  assert(!isPalindromeRec("z"))
  assert(isPalindromeRec("amanaplanacanalpanama"))
  assert(!isPalindromeRec("Test 1,2,3"))
  assert(isPalindromeRec("1 2 1"))
  assert(!isPalindromeRec("A man a plan a canal Panama."))
 
  println("Successfully completed without errors.")
Scheme
Non-recursive 
(define (palindrome? s)
  (let ((chars (string->list s)))
    (equal? chars (reverse chars))))
Recursive 
(define (palindrome? s)
  (let loop ((i 0)
             (j (- (string-length s) 1)))
    (or (>= i j)
        (and (char=? (string-ref s i) (string-ref s j))
             (loop (+ i 1) (- j 1))))))
 
;; Or:
(define (palindrome? s)
  (let loop ((s (string->list s))
             (r (reverse (string->list s))))
    (or (null? s)
        (and (char=? (car s) (car r))
             (loop (cdr s) (cdr r))))))
 
<lang scheme>> (palindrome? "ingirumimusnocteetconsumimurigni")
#t
> (palindrome? "This is not a palindrome")
#f
>
Seed7
const func boolean: palindrome (in string: stri) is func
  result
    var boolean: isPalindrome is TRUE;
  local
    var integer: index is 0;
    var integer: length is 0;
  begin
    length := length(stri);
    for index range 1 to length div 2 do
      if stri[index] <> stri[length - index + 1] then
        isPalindrome := FALSE;
      end if;
    end for;
  end func;
For palindromes where spaces shuld be ignore use: 
palindrome(replace("in girum imus nocte et consumimur igni", " ", ""))
SequenceL
Using the Reverse Library Function 
import <Utilities/Sequence.sl>;
 
isPalindrome(string(1)) := equalList(string, reverse(string));
Version Using an Indexed Function 
isPalindrome(string(1)) :=
        let
                compares[i] := string[i] = string[size(string) - (i - 1)] foreach i within 1 ... (size(string) / 2);
        in
                all(compares);
Sidef
Built-in 
say "noon".is_palindrome;    # true
Non-recursive 
func palindrome(s) {
    s == s.reverse
}
Recursive 
func palindrome(s) {
    if (s.len <= 1) {
        true
    }
    elsif (s.first != s.last) {
        false
    }
    else {
        __FUNC__(s.ft(1, -2))
    }
}
Slate
Non-Recursive 
s@(String traits) isPalindrome
[
  (s lexicographicallyCompare: s reversed) isZero
].
Recursive Defined on Sequence since we are not using String-specific methods: 
s@(Sequence traits) isPalindrome
[
  s isEmpty
    ifTrue: [True]
    ifFalse: [(s first = s last) /\ [(s sliceFrom: 1 to: s indexLast - 1) isPalindrome]]
].
Testing 
define: #p -> 'ingirumimusnocteetconsumimurigni'.
inform: 'sequence ' ; p ; ' is ' ; (p isPalindrome ifTrue: [''] ifFalse: ['not ']) ; 'a palindrome.'.
Smalltalk
Works with: Squeak
isPalindrome := [:aString |
        str := (aString select: [:chr| chr isAlphaNumeric]) collect: [:chr | chr asLowercase].
        str = str reversed.
        ].
 
Works with: GNU Smalltalk
String extend [
  palindro [                  "Non-recursive"
    ^ self = (self reverse)
  ]
  palindroR [                 "Recursive"
    (self size) <= 1 ifTrue: [ ^true ]
      ifFalse: [ |o i f| o := self asOrderedCollection.
          i := o removeFirst.
          f := o removeLast.
          i = f ifTrue: [ ^ (o asString) palindroR ]
                ifFalse: [ ^false ] 
      ]
  ]
].
Testing 
('hello' palindro) printNl.
('hello' palindroR) printNl.
('ingirumimusnocteetconsumimurigni' palindro) printNl.
('ingirumimusnocteetconsumimurigni' palindroR) printNl.
Works with: VisualWorks Pharo Squeak
SequenceableCollection>>isPalindrome
        ^self reverse = self
 
SNOBOL4
        define('pal(str)') :(pal_end)
pal     str notany(&ucase &lcase) = :s(pal)
        str = replace(str,&ucase,&lcase)
        leq(str,reverse(str)) :s(return)f(freturn)
pal_end
 
        define('palchk(str)tf') :(palchk_end)
palchk  output = str; 
        tf = 'False'; tf = pal(str) 'True'
        output = 'Palindrome: ' tf :(return)
palchk_end        
 
*       # Test and display
        palchk('Able was I ere I saw Elba')
        palchk('In girum imus nocte et consumimur igni')
        palchk('The quick brown fox jumped over the lazy dogs')
end
Output:
Able was I ere I saw Elba
Palindrome: True
In girum imus nocte et consumimur igni
Palindrome: True
The quick brown fox jumped over the lazy dogs
Palindrome: False
SQL
SET @txt = REPLACE('In girum imus nocte et consumimur igni', ' ', '');
SELECT REVERSE(@txt) = @txt;
Swift
Works with: Swift version 1.2
import Foundation
 
// Allow for easy character checking
extension String {
    subscript (i: Int) -> String {
        return String(Array(self)[i])
    }
}
 
func isPalindrome(str:String) -> Bool {
    if (count(str) == 0 || count(str) == 1) {
        return true
    }
    let removeRange = Range<String.Index>(start: advance(str.startIndex, 1), end: advance(str.endIndex, -1))
    if (str[0] == str[count(str) - 1]) {
        return isPalindrome(str.substringWithRange(removeRange))
    }
    return false
}
Works with: Swift version 2.0
func isPal(str: String) -> Bool {
  let c = str.characters
  return lazy(c).reverse()
    .startsWith(c[c.startIndex...advance(c.startIndex, c.count / 2)])
}
Tcl
Non-recursive 
package require Tcl 8.5
proc palindrome {s} {
    return [expr {$s eq [string reverse $s]}]
}
Recursive 
proc palindrome_r {s} {
    if {[string length $s] <= 1} {
        return true
    } elseif {[string index $s 0] ne [string index $s end]} {
        return false
    } else {
        return [palindrome_r [string range $s 1 end-1]]
    }
}
Testing 
set p ingirumimusnocteetconsumimurigni
puts "'$p' is palindrome? [palindrome $p]"
puts "'$p' is palindrome? [palindrome_r $p]"
TUSCRIPT
 
$$ MODE TUSCRIPT
pal  ="ingirumimusnocteetconsumimurigni"
pal_r=TURN(pal)
SELECT pal
CASE $pal_r
PRINT "true"
DEFAULT
PRINT/ERROR "untrue"
ENDSELECT
 
Output:
true
Ursala
The algorithm is to convert to lower case, and then compare the intersection of the argument and the set of letters (declared in the standard library) with its reversal. This is done using the built in operator suffixes for intersection (c), identity (i), reversal (x) and equality (E). 
#import std
 
palindrome = ~&cixE\letters+ * -:~& ~=`A-~rlp letters
This test programs applies the function to each member of a list of three strings, of which only the first two are palindromes. 
#cast %bL
 
examples = palindrome* <'abccba','foo ba rra bo of','notone'>
Output:
<true,true,false>
VBA
This function uses function Reverse() (or Rreverse()) from Reverse a string, after first stripping spaces from the string using the built-in function Replace and converting it to lower case. It can't handle punctuation (yet). Just like the VBScript version it could also work using StrReverse. 
 
Public Function isPalindrome(aString as string) as Boolean
dim tempstring as string
  tempstring = Lcase(Replace(aString, " ", ""))
  isPalindrome = (tempstring = Reverse(tempstring))
End Function
 
Example:
print isPalindrome("In girum imus nocte et consumimur igni")
True
VBScript
Implementation
function Squish( s1 )
        dim sRes
        sRes = vbNullString
        dim i, c
        for i = 1 to len( s1 )
                c = lcase( mid( s1, i, 1 ))
                if instr( "abcdefghijklmnopqrstuvwxyz0123456789", c ) then
                        sRes = sRes & c
                end if
        next
        Squish = sRes
end function
 
function isPalindrome( s1 )
        dim squished
        squished = Squish( s1 )
        isPalindrome = ( squished = StrReverse( squished ) )
end function
Invocation
wscript.echo isPalindrome( "My dog has fleas")
wscript.echo isPalindrome( "Madam, I'm Adam.")
wscript.echo isPalindrome( "1 on 1")
wscript.echo isPalindrome( "In girum imus nocte et consumimur igni")
Output:
0
-1
0
-1
Vedit macro language
This routine checks if current line is a palindrome: 
:PALINDROME:
EOL #2 = Cur_Col-2
BOL
for (#1 = 0; #1 <= #2/2; #1++) {
    if (CC(#1) != CC(#2-#1)) { Return(0) }
}
Return(1)
Testing: 
Call("PALINDROME")
if (Return_Value) {
    Statline_Message("Yes")
} else {
    Statline_Message("No")
}
Return
Wortel
@let {
  ; Using a hook
  pal1 @(= @rev)
  ; Function with argument
  pal2 &s = s @rev s
  ; for inexact palindromes
  pal3 ^(@(= @rev) .toLowerCase. &\@replace[&"\s+"g ""])
  [[
    !pal1 "abcba"
    !pal2 "abcbac"
    !pal3 "In girum imus nocte et consumimur igni"
  ]]
}
Returns: 
[true false true]
XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings
 
func StrLen(Str);               \Return number of characters in an ASCIIZ string
char Str;
int  I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;
 
func Palindrome(S);             \Return 'true' if S is a palindrome
char S;
int  L, I;
[L:= StrLen(S);
for I:= 0 to L/2-1 do
        if S(I) # S(L-1-I) then return false;
return true;
]; \Palindrome
 
int Word, I;
[Word:=
    ["otto", "mary", "ablewasiereisawelba", "ingirumimusnocteetconsumimurigni"];
for I:= 0 to 4-1 do
    [Text(0, if Palindrome(Word(I)) then "yes" else "no");
    CrLf(0);
    ];
]
Output:
yes
no
yes
yes
Yorick
Function is_palindrome meets the task description. Function prep_palindrome demonstrates how to convert an English sentence into a form that can be tested with is_palindrome (by changing case and stripping non-alphabetical characters). 
func is_palindrome(str) {
    s = strchar(str)(:-1);
    return allof(s == s(::-1));
}
 
func prep_palindrome(str) {
    s = strchar(strlower(str));
    w = where(s >= 'a' & s <= 'z');
    return strchar(s(w));
}
zkl


fcn pali(text){
   if (text.len() < 2) return(False);
   [0..text.len()/2].filter1('wrap(n){text[n] != text[-n-1]}) :
   return(_ == False);
}
Walk from both ends of the string (to the middle) until a mismatch is seen. 'wrap creates a closure function over the unknowns in the function (in this case, text) (functions are not lexically scoped) 
Output:
pali("red rum sir is murder") //--> False
pali("red rum sir is murder"-" ") //-->True, remove spaces
pali("In girum imus nocte et consumimur igni".toLower()-" ") //-->True


