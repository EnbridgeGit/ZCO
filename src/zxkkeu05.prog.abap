*----------------------------------------------------------------------*
*   INCLUDE ZXKKEU05                                                   *
*----------------------------------------------------------------------*
E_CHAR_GROUP = I_CHAR_GROUP.


CASE I_ERKRS.

  WHEN '1100'.
    IF IS_COBL-VORGN EQ 'RFBU' AND IS_COBL-bukrs EQ 'MNPP'.
      E_CHAR_GROUP = 'MNPP'.
    ENDIF.

    IF IS_COBL-VORGN EQ 'RFBU' AND
      ( IS_COBL-bukrs EQ 'UCIS' OR IS_COBL-bukrs EQ 'INLX' ).
      E_CHAR_GROUP = 'UCIS'.
    ENDIF.

    IF IS_COBL-VORGN EQ 'RFBU' AND
      ( IS_COBL-bukrs EQ 'EMP ' OR IS_COBL-bukrs EQ 'EUSA' ).
      E_CHAR_GROUP = 'EMP'.
    ENDIF.

ENDCASE.

