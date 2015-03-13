# erl4th
Forth in Erlang

�B�� Erlang �y�����S�ʡA�� Forth �y���i�H�B�@�C

�n�sĶ Forth �A�@�}�l�u�n�����J���{���ΪŮ���}�A�N�w�g�O��Ǧ��F�A�ӫ�Ǧ��]�O�@�ؤG����Φh����C�M��A Forth �{���Ϥ����T�زŸ��G��r�B�ȡB�Ϲj�Ÿ��A�ҥH�� lexical analysis �ɡA���ӭn�o�˰��G

* ��r���ӭn��ܬ��q��r�A�ѼƼƥءr�A�]����r�O�B��Ÿ��A���۹諸�ѼơA�ӰѼƼƥت�ܬ��t��ơC�Ҧp�qmy-emit, -1�r�A�۷��b��ƻy�������Ө�ƦW�s my-emit/1 �C
* �����ӭn��ܬ��q�ȡA���ưѼƼƥءr�C�Ȫ��s�b�A�i�ध��|�Q�B��Ÿ����Ʊ��A�ҥH�ݭn�w�q�u���ưѼƼƥءv�A�h��q��r�A�ѼƼƥءr���[�`����C��O�A�p�G���@�ӹB��Ÿ����u�ѼƼƥءv�S������� 0 �A�N�i�H�L�X���ӹB��Ÿ��O stack underflow error �C
* �Ϲj�Ÿ��u��Ӽ˦s�b lexical analysis �����G�A�]�����ᰵ semantic analysis �n�ΡC

Syntax analysis �N�����F�A�]�� Forth �{���g lexical analysis ����A�o��@��F��A�N�O�y�k��C

�ܩ� semantic analysis �����ȡA�O�� Forth �{����z���@���r��M�@�ӹB��Ϊ� stack �C�r�夤���C�@���A�O�@�Ӥ�r�w�q�A�ӥB�w�q�n����r���N�O��ơA�Ҧp : star 42 emit ; �O��� start/0 �C�b�o�@���q�A�n��C�@����r���ѼƼƥغ�X�ӡA�ҥH�@����r��F�� (key, value) �A�h�O�]�qstar, 0�r�A[42, emit]�^�A���� lexem value �O [�q42, 1�r, �qemit, -1�r] �A��Ӹg�� semantic analysis �B�z����A�n��ĤG�ӼƦr�����A�ܦ� [42, emit] �A����i�H�������h��C