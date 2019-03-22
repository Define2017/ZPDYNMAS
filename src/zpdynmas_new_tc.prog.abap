*&---------------------------------------------------------------------*
*&  Include  ZPDYNMAS_NEW_TC
*&---------------------------------------------------------------------*
MODULE tc_t588z_change_tc_attr OUTPUT.
  DESCRIBE TABLE t588z_itab LINES tc_t588z-lines.
ENDMODULE.

MODULE pbo_2000 OUTPUT.

  DATA excl_tab TYPE TABLE OF sy-ucomm.

  DATA(excl) = COND #( WHEN lcl_general_view=>get_instance( )->get_active_read_mode( ) EQ abap_true THEN 'DISP'
                       ELSE 'EDIT' ).

  APPEND excl TO excl_tab.

  SET PF-STATUS 'MAIN' EXCLUDING excl_tab.
  SET TITLEBAR 'STD'.

  lcl_general_view=>pbo( ).

  CLEAR excl_tab.

ENDMODULE.

MODULE pai_2000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'INSR'.
      " call insert method of lcl_general_views
      lcl_general_view=>get_instance( )->insert( ).
    WHEN 'DEL'.
      lcl_general_view=>get_instance( )->delete( ).
    WHEN 'MAL'.
      lcl_general_view=>get_instance( )->mark_all( ).
    WHEN 'UAL'.
      lcl_general_view=>get_instance( )->unmark_all( ).
    WHEN 'DISP'.
      lcl_general_view=>get_instance( )->change_mode( read_mode = abap_true ).
    WHEN 'EDIT'.
      lcl_general_view=>get_instance( )->change_mode( write_mode = abap_true ).
    WHEN 'NUMB'.
      lcl_table_view=>rebuild_numbers( ).
    WHEN 'SAVE'.
      lcl_general_view=>get_instance( )->save_changes( ).
  ENDCASE.
ENDMODULE.

MODULE tc_t588z_modify INPUT.
  MODIFY t588z_itab FROM t588z_wa INDEX tc_t588z-current_line.
ENDMODULE.
