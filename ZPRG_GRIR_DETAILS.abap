REPORT  ZPRG_GRIR_DETAILS LINE-SIZE 1000.

TABLES: EKBE, BKPF, EKPO, EKKN, AFKO, MSEG.


PARAMETERS: p_budat like bkpf-bldat default '20111231'.
PARAMETERS: p_file like rlgrap-filename default '\\172.18.1.56\dev\GRIR_Extract.txt'.
*PARAMETERS: p_doctp like bkpf-blart default 'WE'.
DATA: BEGIN OF it_po OCCURS 0,
      ebeln like ekko-ebeln, "PO number
      lifnr like ekko-lifnr, "Vendor Number
      name1 like lfa1-name1, "Vendor Name
      END OF it_po.

DATA: tmp_total like bsis-dmbtr,
      tmp_total2(18),
      tmp_bewtp like ekbe-bewtp,
      tmp_netpr like ekpo-netpr,
      tmp_menge like ekbe-menge,
      tmp_wrbtr like ekbe-wrbtr.

DATA: BEGIN OF it_line OCCURS 0,
      acc_doc_num like bkpf-belnr, "Document Number
      belnr like ekbe-belnr, "Material Document Number
      buzei like ekbe-buzei, "Material Line Item Number
      projdef like proj-pspid, "Project Definition
      gjahr like ekbe-gjahr, "Fiscal Year
      total(18), "Total Amount of Document
      bewtp like ekbe-bewtp, "Document Type
      budat like ekbe-budat, "Posting Date
      menge(18), "Quantity
      ebelp like ekbe-ebelp, "PO Line Item
      wrbtr(18), "Amount Charged
      activity like ekpo-txz01, "Activity
      netpr(18), "PO Net Price per Unit
      network like ekkn-nplnr, "Network
      pronr like afko-pronr, "Project Definition (2)
      erfme like mseg-erfme, "Unit of Entry
      END OF it_line.

DATA: r_acc_doc type range of bkpf-belnr,
      wa_acc_doc like line of r_acc_doc.

DATA: file_line(1000),
      header_line(1000).

select ebeln lifnr
  into (it_po-ebeln, it_po-lifnr)
  from ekko
  where bukrs EQ 'MWSI'
  and bsart EQ 'PMG'
  and bstyp EQ 'F'.

  select single name1
    into (it_po-name1)
    from lfa1
    where lifnr EQ it_po-lifnr.

  append it_po.
endselect.


OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

*DELETE it_po where ebeln NE '4500000001'.

DATA: tmp_awkey like bkpf-awkey.

loop at it_po.

  refresh it_line[].
  clear it_line.

  select ebelp belnr buzei gjahr bewtp budat menge wrbtr
    into (it_line-ebelp, it_line-belnr, it_line-buzei, it_line-gjahr, it_line-bewtp, it_line-budat, tmp_menge, tmp_wrbtr)
    from ekbe
    where ebeln EQ it_po-ebeln
    and budat LE p_budat
    and ( bewtp EQ 'E' OR bewtp EQ 'Q' ).

    it_line-menge = tmp_menge.
    it_line-wrbtr = tmp_wrbtr.
    CONCATENATE it_line-belnr it_line-gjahr into tmp_awkey.

    select single belnr
      into (it_line-acc_doc_num)
      from bkpf
      where awkey EQ tmp_awkey.


    select single txz01 netpr
      into (it_line-activity, tmp_netpr)
      from ekpo
      where ebeln EQ it_po-ebeln
      and ebelp EQ it_line-ebelp.

    it_line-netpr = tmp_netpr.

    select single nplnr
      into it_line-network
      from ekkn
      where ebeln EQ it_po-ebeln
      and ebelp EQ it_line-ebelp.

    select single pronr
      into it_line-pronr
      from afko
      where aufnr EQ it_line-network.

    select single pspid
      into it_line-projdef
      from proj
      where pspnr EQ it_line-pronr.

    select single erfme
      into it_line-erfme
      from mseg
      where mblnr EQ it_line-belnr
      and zeile EQ it_line-buzei.

    append it_line.
  endselect.

  SORT it_line ASCENDING by budat acc_doc_num ebelp.



  if sy-subrc EQ 0.
    write:/ it_po-lifnr, it_po-name1, it_po-ebeln.

    TRANSFER 'Vendor No|Vendor Name|PO Number|Document Date|Document Number|Total|Material Document|Project Definition|Activity|PO Line Item|PO Net Price per Unit|Quantity Charged|UM|Amount Charged' to p_file.

    loop at it_line.

      tmp_bewtp = it_line-bewtp.

      at new acc_doc_num.

*        if it_line-acc_doc_num NOT IN r_acc_doc OR r_acc_doc IS INITIAL.
          case tmp_bewtp.
            when 'Q'.
              select sum( dmbtr )
                into tmp_total
                from bsis
                where bukrs EQ 'MWSI'
                and belnr EQ it_line-acc_doc_num
                and bschl EQ '86'.
            when 'E'.
              select sum( dmbtr )
                into tmp_total
                from bsis
                where bukrs EQ 'MWSI'
                and belnr EQ it_line-acc_doc_num
                and bschl EQ '96'.
          endcase.

*          wa_acc_doc-sign = 'I'.
*          wa_acc_doc-option = 'EQ'.
*          wa_acc_doc-low = it_line-acc_doc_num.
*          append wa_acc_doc to r_acc_doc.


          tmp_total2 = tmp_total.
*        endif.
        skip.
      endat.


      write:/ '--------', it_line-budat, it_line-acc_doc_num, tmp_total, it_line-belnr, it_line-projdef, it_line-bewtp, it_line-ebelp, it_line-network, it_line-pronr, it_line-netpr, it_line-menge, it_line-wrbtr, it_line-erfme, it_line-activity.

      CONCATENATE it_po-lifnr '|' it_po-name1 '|' it_po-ebeln '|' it_line-budat '|' it_line-acc_doc_num '|' tmp_total2 '|' it_line-belnr '|' it_line-projdef '|' it_line-activity '|' it_line-ebelp '|' it_line-netpr '|' it_line-menge '|' it_line-erfme '|'
it_line-wrbtr into file_line.


      TRANSFER file_line to p_file.
      CLEAR file_line.

    endloop.
  endif.
  skip.


endloop.

CLOSE DATASET p_file.
