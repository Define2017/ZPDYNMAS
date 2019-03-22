*&---------------------------------------------------------------------*
*& Report  ZPDYNMAS_NEW
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zpdynmas_new.

TYPES: BEGIN OF t_t588z,
         flag TYPE flag.
        INCLUDE STRUCTURE t588z.
TYPES: END OF t_t588z.

CONTROLS: tc_t588z TYPE TABLEVIEW USING SCREEN 2000.
DATA t588z_itab TYPE TABLE OF t_t588z.
DATA t588z_wa TYPE t_t588z.

INCLUDE zpdynmas_new_cls.
INCLUDE zpdynmas_new_tc.

START-OF-SELECTION.

  lcl_dynmas=>get_instance( )->select_dynmas( ).

END-OF-SELECTION.

  CALL SCREEN 2000.
