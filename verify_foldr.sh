#!/bin/bash
# verify_foldr.sh
# Verification script for foldr implementation

echo "════════════════════════════════════════════════════════"
echo "  FOLDR IMPLEMENTATION VERIFICATION"
echo "════════════════════════════════════════════════════════"
echo ""
echo "Problem Statement:"
echo "  Implement foldr(string_concat, reverse(string_chars(\"abc\")), \"\", B)."
echo "  Expected: B = \"cba\""
echo ""
echo "────────────────────────────────────────────────────────"
echo "Test 1: Pure Prolog syntax"
echo "────────────────────────────────────────────────────────"

swipl -g "use_module(starlog), string_chars(\"abc\", Chars), reverse(Chars, RevChars), foldr(string_concat, RevChars, \"\", B), format('Result: B = \"~w\"~n', [B]), (B = \"cba\" -> write('✓ PASS') ; write('✗ FAIL')), nl, halt" 2>&1 | grep -E "(Result:|PASS|FAIL)"

echo ""
echo "────────────────────────────────────────────────────────"
echo "Test 2: Starlog syntax with nested functions"
echo "────────────────────────────────────────────────────────"

swipl -g "use_module(starlog), starlog_call(B is foldr(string_concat, reverse(string_chars(\"abc\")), \"\")), format('Result: B = \"~w\"~n', [B]), (B = \"cba\" -> write('✓ PASS') ; write('✗ FAIL')), nl, halt" 2>&1 | grep -E "(Result:|PASS|FAIL)"

echo ""
echo "────────────────────────────────────────────────────────"
echo "Test 3: Different input strings"
echo "────────────────────────────────────────────────────────"

swipl -g "use_module(starlog), starlog_call(B1 is foldr(string_concat, reverse(string_chars(\"hello\")), \"\")), format('\"hello\" -> \"~w\" ', [B1]), (B1 = \"olleh\" -> write('✓') ; write('✗')), nl, starlog_call(B2 is foldr(string_concat, reverse(string_chars(\"xyz\")), \"\")), format('\"xyz\" -> \"~w\" ', [B2]), (B2 = \"zyx\" -> write('✓') ; write('✗')), nl, halt" 2>&1 | grep -E "(hello|xyz)"

echo ""
echo "────────────────────────────────────────────────────────"
echo "Test 4: foldr with list append"
echo "────────────────────────────────────────────────────────"

swipl -g "use_module(starlog), foldr(append, [[1], [2], [3]], [], Result), format('Result: ~w~n', [Result]), (Result = [1,2,3] -> write('✓ PASS') ; write('✗ FAIL')), nl, halt" 2>&1 | grep -E "(Result:|PASS|FAIL)"

echo ""
echo "────────────────────────────────────────────────────────"
echo "Test 5: Empty list edge case"
echo "────────────────────────────────────────────────────────"

swipl -g "use_module(starlog), foldr(string_concat, [], \"init\", Result), format('Result: ~w~n', [Result]), (Result = \"init\" -> write('✓ PASS') ; write('✗ FAIL')), nl, halt" 2>&1 | grep -E "(Result:|PASS|FAIL)"

echo ""
echo "════════════════════════════════════════════════════════"
echo "  VERIFICATION COMPLETE"
echo "════════════════════════════════════════════════════════"
