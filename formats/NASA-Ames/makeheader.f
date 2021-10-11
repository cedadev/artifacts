c August 3rd, 2000.
c
c Contact: Anne De Rudder
c
c British Atmospheric Data Centre, Rutherford Appleton Laboratory
c Chilton, Didcot, Oxfordshire, OX11 0QX, United Kingdom
c Tel.: +44 (0) 1235 446432. - Fax.: +44 (0) 1235 445848.
c E-mail: badc@rl.ac.uk
c     
c
c                             FOREWORD TO THE USER
c                             ====================
c
c     This Fortran program generates the appropriate header of a NASA Ames
c     formatted file, from the information provided by the user. The used
c     nomenclature and vocabulary refer to Gaines and Hipskind, 1998 (*). All
c     variables in capital letters have the same name as in this document, that
c     can be consulted online or downloaded at
c     http://www.badc.rl.ac.uk/data/utls-ozone/forspec.txt
c     A summary of NASA Ames formats is also provided on line at
c     http://www.badc.rl.ac.uk/data/urgent/NASA-Ames/FFI-summary.html
c
c     NASA Ames makes provision of 9 data formats that cover most plausible
c     needs of atmospheric scientists. These formats are not generic enough to
c     be suitable to all cases. If your data do not fit into any of the provided
c     formats, please contact BADC (badc@rl.ac.uk).
c
c     To generate a complete NASA Ames file, you only need to add the data at
c     the bottom of the file issued by the present program, or to insert the
c     header at the top of your data file. Please make sure your actual data
c     content and format correspond to the information you provide to the
c     program that produces the header (if this is not the case, the file will
c     be rejected on submission).
c
c     Since this program is primarily intended to the participants to the URGENT
c     NERC Thematic Programme, it is based on the URGENT file naming rules,
c     which are different from the NASA Ames file naming rules. For information
c     on URGENT file names, please refer to the online documentation at
c     http://www.badc.rl.ac.uk/data/urgent/FileNames.html
c     If this program is used in another framework than the URGENT Programme,
c     the output file might need to be renamed. Alternatively, the section of
c     the program where the data file name (filena) is generated may be adapted.
c
c     (*) S.E. Gaines & R.S. Hipskind, Format Specification for Data Exchange,
c         Version 1.2, 12 January 1998.
c
c                            PRACTICAL INSTRUCTIONS
c                            ======================
c
c     Before using the program, save a copy of the present template.
c
c     Please input the required information in the two frames A and B included
c     between the INPUT START and INPUT END lines below. Proceed through all
c     numbered instructions from 1 to 35.
c
c     Note that in Fortran, all variables need to be defined. If some of the
c     variables do not apply to your case, please use the default definition
c     provided.
c
c     Unless you wish to make the program perform a different task, please do
c     not change anything to it outside of the INPUT frames. Do not remove the
c     quotes around character strings. Do not remove the initial 'c' of comment
c     lines. If names (ONAME, ORG, SNAME, MNAME) include apostrophes, double
c     them (i.e., write '' instead of ').
c
c     Fortran instructions must remain within row 7 to row 72. If an instruction
c     extends over more than one line, please use any sign in row 6 of the
c     subsequent lines (not in the first one) to indicate a follow-up. A rule
c     indicating row numbers is inserted at the top of each INPUT frame.
c
c     Once you have input all the required information, save the source file,
c     compile the program and execute it.
c     On a Unix machine:
c
c     f77 makeheader.f
c     a.out
c
c     Unless execution fails and, in some cases, even when it fails (in which
c     case you should get an error message), the program will issue an output
c     file with a name ending with '.na'. If execution has been successful, this
c     file contains the header associated to your data.
c
c     If you need to execute the program again with the same data file name,
c     first make sure that you have deleted the former output file from your
c     directory. Otherwise, execution will abort and a warning will tell you
c     that you are trying to overwrite an existing file.
c
c                      SOME NASA AMES RULES AND TERMINOLOGY
c                      ====================================
c
c     It is a NASA Ames convention that the dimension (number of values) of the
c     independent variable that varies the most slowly in the data record is not
c     explicitly defined in the data file. This is why that variable is called
c     "unbounded". All the other independent variables, when any, are "bounded",
c     i.e. their dimension is explicitly recorded in the data file.
c
c     The unbounded independent variable usually takes real values, but NASA
c     Ames allows it to be a character string if there is only one other
c     independent variable (which must be real).
c
c     In some cases, measurements or calculations may be made at values of the
c     bounded variable(s) that depend on the unbounded variable values (for
c     example, on altitude ranges that depend on flight time). Although it may
c     seem paradoxical that independent variables may depend on each other, it
c     must be noted that they are still independent in a physical sense. Only
c     the chosen discrete subsets of values they take depend on each other.
c
c     A quantity or variable of which the values are/is recorded in the data
c     file is called "explicit". If it is not explicit but if its value(s) can
c     be deduced from the information included in the file, it is "implicit".
c     For reasons of economy and when possible, variables may be totally or
c     partially implicit.
c     --------------------------------------------------------------------------

      program makeheader

      integer NIV, NV, nav, NAUXC, NSCOML, NNCOML, nx1, nx2, nx3
      integer ni, nar, nac, maxnx


c     ==================================================================
c  A. INPUT START (A)
c     ===========
c
c        1         2         3         4         5         6         7  
c     789012345678901234567890123456789012345678901234567890123456789012
c     ...!....!....!....!....!....!....!....!....!....!....!....!....!..
c 
c     ------------------------------------------------------------------ 
c  1. NIV: number of independent variables (from 1 to 4).
c     ------------------------------------------------------------------ 

      parameter (NIV=3)

c     ------------------------------------------------------------------ 
c  2. NV: number of primary dependent variables (NV > 0).
c     ------------------------------------------------------------------ 

      parameter (NV=2)

c     ------------------------------------------------------------------ 
c  3. nav: number of real auxiliary dependent variables.
c     nav > or = 0.
c     Note that if there are two independent variables (NIV=2) and if
c     the bounded variable values depend on the unbounded variable
c     (ndep=1 below), then there must be at least one auxiliary variable
c     (the first one), which is the number of values of the bounded
c     independent variable (it is an integer but is considered here as a
c     real number). Furthermore, in this case, if the bounded
c     independent variable is regularly spaced and if you want it to be
c     implicit (nimp=1 below) (this option is only possible with a real
c     unbounded variable), then the second and third auxiliary variables
c     must be the base value and the increment of the bounded
c     independent variable respectively.
c     ------------------------------------------------------------------ 

      parameter (nav=1)

c     ------------------------------------------------------------------ 
c  4. NAUXC: number of auxiliary dependent variables that are character
c     strings.
c     NAUXC > or = 0.
c     Note that the option NAUXC > 0 is only offered when the unbounded
c     independent variable is a character string itself.
c     ------------------------------------------------------------------ 

      parameter (NAUXC=1)

c     ------------------------------------------------------------------ 
c  5. Number of values of the bounded variables.
c     For i = 1, 2 or 3, nxi is an integer.
c     * If there are more than i independent variables and if the ith
c       bounded variable values do not depend on the unbounded
c       independent variable, then nxi is the number of values of the
c       ith (bounded) independent variable.
c     * In all other cases, nxi=1.
c     ------------------------------------------------------------------ 

      parameter (nx1=5, nx2=4, nx3=1)

c     ------------------------------------------------------------------ 
c  6. NSCOML: Number of special comment lines.
c     ------------------------------------------------------------------ 

      parameter (NSCOML=4)

c     ------------------------------------------------------------------ 
c  7. NNCOML: Number of normal comment lines.
c     ------------------------------------------------------------------ 

      parameter (NNCOML=7)

c     =========
c     INPUT END (A)
c     ==================================================================

      parameter (ni=NIV-1, nar=nav+1, nac=NAUXC+1)
      parameter (maxnx=max(nx1,nx2,nx3))

      integer       nimp, ncha, ndep
      integer       IVOL, NVOL, NVPM, LENX
      integer       year, month, day, ryear, rmonth, rday
      integer       LENA(nac)
      real          x1(nx1), x2(nx2), x3(nx3)
      real          DX(NIV), VSCAL(NV), VMISS(NV)
      real          ASCAL(nar), AMISS(nar)
      character*132 ONAME, ORG, SNAME, MNAME
      character*132 XNAME(NIV), VNAME(NV), ANAME(nar)
      character*132 cname(nac), cmiss(nac)
      character*132 SCOM(NSCOML), NCOM(NNCOML)

      integer       i, j, iu, yy, iflag, lblank, aux
      integer       miv, mv, mav, mauxc, mscoml, mncoml
      integer       mimp, mcha, mdep, mx1, mx2, mx3, mx
      integer       endce, endda, endon, endor, endsn, endmn
      integer       endxn(NIV), endvn(NV)
      integer       endan(nar), endcm(nac), endcn(nac)
      integer       FFI, iv2, iv3, NLHEAD, NAUXV
      integer       NX(3), NXDEF(3), X(maxnx,3)
      character*6   ymd
      character*10  DATE, RDATE
      character*40  filena
      character*132 CentreID, DataID, string

c     ==================================================================
c  B. INPUT START (B)
c     ===========
c
c        1         2         3         4         5         6         7  
c     789012345678901234567890123456789012345678901234567890123456789012
c     ...!....!....!....!....!....!....!....!....!....!....!....!....!..
c 
c     ------------------------------------------------------------------ 
c  8. When possible, would you like your indpdt variable(s) to be implicit?
c     No  <-> nimp=0  (all independent variables are explicit)
c     Yes <-> nimp=1
c     ------------------------------------------------------------------ 

      parameter (nimp=1)

c     ------------------------------------------------------------------ 
c  9. Is your unbounded variable a character string?
c     No  <-> ncha=0
c     Yes <-> ncha=1
c     ------------------------------------------------------------------ 

      parameter (ncha=0)

c     ------------------------------------------------------------------ 
c 10. Are some of your bounded independent variables defined over a
c     domain of values that depends on the values of the unbounded
c     independent variable?
c     No (or inappropriate) <-> ndep=0
c     Yes                   <-> ndep=1
c     ------------------------------------------------------------------ 

      parameter (ndep=0)

c     ------------------------------------------------------------------ 
c 11. XNAME: Independent variable(s) name(s) and unit(s).
c     XNAME is an array of NIV components: XNAME(s), s=1,NIV.
c     XNAME(s) is a character string (maximum 132 characters long)
c     giving the name and/or description of the independent variable
c     number s, and its unit of measure.
c     The values of XNAME must be ordered according to the speed of
c     variation of the corresponding independent variables in the actual
c     data records: the first listed independent variable (s=1) is the
c     one that varies the fastest, the last listed independent variable
c     (s=NIV) is the most slowly varying one.
c     ------------------------------------------------------------------ 

      data XNAME / 'First bounded independent variable',
     >             'Second bounded independent variable',
     >             'Unbounded independent variable' /

c     ------------------------------------------------------------------ 
c 12. DX: Interval identifier for the independent variable(s).
c     DX is an array of NIV real components: DX(s), s=1,NIV.
c     The components of DX must be ordered like the components of XNAME.
c     DX(s) is non zero <-> Successive values of the independent
c                           variable number s are separated by a
c                           constant interval equal to DX(s), expressed
c                           in the unit specified in XNAME(s).
c     DX(s) = 0.        <-> Interval between successive values of
c                           independent variable number s is not
c                           constant or X is a character string. 
c     ------------------------------------------------------------------ 

      data DX / 0., -0.5, 0. /

c     ------------------------------------------------------------------ 
c 13. Bounded variables.
c     For i = 1, 2 or 3, xi is an array of nxi real components (refer to
c     the value of nxi above).
c     * If nxi > 1 and
c       - if the ith bounded variable values are irregularly spaced
c         [DX(i)=0.], then xi(j) (j=1,nxi) are the values of the ith
c         independent variable;
c       - if the ith bounded variable values are regularly spaced [DX(i)
c         not equal to 0.], then
c         xi(1) is the first value of the ith independent variable and,
c         for 1 < j < nxi+1, xi(j)=0.
c       Note that you can write 10*0. instead of 0., 0., ..., 0. (10
c       times).
c     * If nxi=1, xi is a real number equal to 0.
c     ------------------------------------------------------------------ 

      data x1 / -6.0, -3.0, -1.5, 0.0, 2.0  /
      data x2 / 10., 3*0. /
      data x3 / 0. /

c     ------------------------------------------------------------------ 
c 14. VNAME: Primary dependent variable(s) name(s) and unit(s).
c     VNAME is an array of NV components: VNAME(n), n=1,NV.
c     VNAME(n) is a character string (maximum 132 characters long)
c     giving the name and/or description of the primary dependent
c     variable number n, and its unit of measure when its recorded
c     values will be multiplied by the specified scale factor (see VSCAL
c     below).
c     The order of the values of VNAME must correspond to the order of
c     the primary variables in the actual data records.
c     ------------------------------------------------------------------ 

      data VNAME / 'First primary variable',
     >             'Second primary variable' /

c     ------------------------------------------------------------------ 
c 15. VSCAL: Scaling factor(s) for the primary variable(s).
c     VSCAL is an array of NV real components: VSCAL(n), n=1,NV.
c     Its components must be ordered like the components of VNAME.
c     VSCAL(n) is the number by which the values of the nth recorded
c     primary variable must be multiplied in order to represent its
c     expression in the unit specified in VNAME(n).
c     ------------------------------------------------------------------ 

      data VSCAL / 1., 1.e12 /

c     ------------------------------------------------------------------ 
c 16. VMISS: Missing value identifier(s) for the primary variable(s).
c     VMISS is an array of NV real components: VMISS(n), n=1,NV.
c     Its components must be ordered like the components of VNAME.
c     VMISS(n) is the number that will indicate a missing or bad value
c     in the data record of primary variable number n. It must be
c     greater than the maximum recorded value for that variable (before
c     scaling).
c     ------------------------------------------------------------------ 

      data VMISS / 999999., 999. /

c     ------------------------------------------------------------------ 
c 17. ANAME: Real auxiliary dependent variable(s) name(s) and unit(s).
c     ANAME is an array of nav+1 components: ANAME(n), n=1,nav+1.
c     For n < nav + 1, ANAME(n) is a character string (maximum 132
c     characters long) giving the name and/or description of the
c     auxiliary dependent variable number n, and its unit of measure
c     when its recorded values will be multiplied by the specified scale
c     factor (see ASCAL below).
c     The order of the values of ANAME must correspond to the order of
c     the auxiliary variables in the actual data records.
c     For n = nav + 1, ANAME(nav+1) is a blank line of 1 character.
c     ------------------------------------------------------------------ 

      data ANAME / 'First real auxiliary variable',
     >             ' ' /

c     ------------------------------------------------------------------ 
c 18. ASCAL: Scaling factor(s) for the auxiliary variable(s).
c     ASCAL is an array of nav+1 real components: ASCAL(n),
c     n=1,nav+1.
c     Its components must be ordered like the components of ANAME.
c     ASCAL(n) is the number by which the values of the nth recorded
c     auxiliary variable must be multiplied in order to represent its
c     expression in the unit specified in ANAME(n).
c     ASCAL(nav+1) can be set to any arbitrary real value.
c     ------------------------------------------------------------------ 

      data ASCAL / 1., 1. /

c     ------------------------------------------------------------------ 
c 19. AMISS: Missing value identifier(s) for the auxiliary variable(s).
c     AMISS is an array of nav+1 real components: AMISS(n),
c     n=1,nav+1.
c     Its components must be ordered like the components of ANAME.
c     AMISS(n) is the number that will indicate a missing or bad value
c     in the data record of auxiliary variable number n. It must be
c     greater than the maximum recorded value for that variable (before
c     scaling).
c     AMISS(nav+1) can be set to any arbitrary real value.
c     ------------------------------------------------------------------ 

      data AMISS / 99., 1. /

c     ------------------------------------------------------------------ 
c 20. LENA: Length of auxiliary variables that are character strings.
c     LENA is an array of NAUXC+1 integer components.
c     For 0 < n < NAUXC+1, LENA(n) is the number of characters of the
c     nth character string variable; it must be smaller than 133.
c     LENA(NAUXC+1) may be set to any arbitrary integer value.
c     LENA is used only when the unbounded independent variable is a
c     character string itself and NAUXC > 0. Otherwise, LENA(n) can be
c     set to any arbitrary integer.
c     ------------------------------------------------------------------ 

      data LENA / 6, 0 /

c     ------------------------------------------------------------------ 
c 21. cname: Name(s) of the auxiliary variable(s) that are character
c     strings. cname is an array of NAUXC+1 components.
c     cname(n) must not be more than 132 characters long.
c     cname(NAUXC+1) is a blank string.
c     ------------------------------------------------------------------ 

      data cname / 'First character auxiliary variable',
     >             ' ' /

c     ------------------------------------------------------------------ 
c 22. cmiss: Missing value identifier(s) for the auxiliary variable(s)
c     that are character strings.
c     cmiss is an array of NAUXC+1 components.
c     cmiss(n) is the string that will indicate a missing or bad value
c     in the data record of the character auxiliary variable number n
c     (maximum 132 characters).
c     cmiss(NAUXC+1) is a blank string.
c     ------------------------------------------------------------------ 

      data cmiss / 'zzzzzz', ' ' /

c     ------------------------------------------------------------------ 
c 23. SCOM: Special comment lines (from 1 to NSCOML).
c            Each line must not be more than 132 characters long.
c     ------------------------------------------------------------------ 

      data SCOM / 'For J(O1D), both radiometers were facing upwards.',
     >   'For J(NO2), the 2*pi radiometer was facing upwards while the',
     >   '4*pi radiometer was facing both upwards and downwards.',
     >   'THIS-FILE-ENDS = 1999 12 31' /

c     ------------------------------------------------------------------ 
c 24. NCOM: Normal comment lines (from 1 to NNCOML).
c            Each line must not be more than 132 characters long.
c     ------------------------------------------------------------------ 

      data NCOM / 'SITE-NAME = Pritchatts Road',
     >            'LATITUDE = 52.3 deg',
     >            'LONGITUDE = -1.56 deg',
     >            'E-MAIL-CONTACT = mk46@leicester.ac.uk',
     >            ' ',
     >            'UT(days)    J(O1D) (Hz)    J(NO2) (Hz)',
     >            ' ' /

c     ------------------------------------------------------------------ 
c 25. If you have only one independent variable, the values of which are
c     regularly spaced, and if you wish it to be partly implicit (imp=1
c     above), input here the number of intervals between 2 consecutive
c     marks of your independent variable. NVPM is an integer such that
c     X(m)-X(m-1)=NVPM*DX for all valid m, where X(m-1) and X(m) are
c     consecutive marks of X and DX is the variable interval.
c     Note that this implies that NVPM values of each primary variable
c     will be recorded on a single line (FFI 1020).
c     NVPM will not be used in other cases (but must still be set to an
c     arbitrary integer value0.
c     ------------------------------------------------------------------ 

      NVPM=6

c     ------------------------------------------------------------------ 
c 26. If the unbounded independent variable is a character string, LENX
c     is its maximal length (number of characters). Otherwise, LENX is
c     not used (but must be set to an arbitrary integer value).
c     0 < LENX < 133.
c     ------------------------------------------------------------------ 

      LENX=30

c     ------------------------------------------------------------------ 
c 27. First 2 fields of data file name.
c     Please read the BADC instructions on URGENT file naming.
c     PLEASE KEEP length(CentreID) + length(DataID) < 30 characters.
c     Example: CentreID='pritchatts-leic'
c              DataID='jo1d-jno2'
c     ------------------------------------------------------------------ 

      CentreID='Centre'
      DataID='Data'

c     ------------------------------------------------------------------ 
c 28. List of author(s) (last name first, followed by first name)
c     separated by semi-colons.
c     Please restrict this line to maximum 132 characters.
c     ------------------------------------------------------------------ 

      ONAME='Lastname, Firstname'

c     ------------------------------------------------------------------ 
c 29. Input the name of your organisation or university and its address.
c     Please restrict this line to maximum 132 characters.
c     ------------------------------------------------------------------ 

      ORG='University, Number, Street, City, Post code, Country'

c     ------------------------------------------------------------------ 
c 30. Specify here the source of the measurements or model results you
c     are submitting: instrument and/or platform, model name, etc.
c     Please restrict this line to maximum 132 characters.
c     ------------------------------------------------------------------ 

      SNAME='Instrument, Platform'

c     ------------------------------------------------------------------ 
c 31. Specify here the name(s) of the programme, project and/or mission.
c     Examples: MNAME='URGENT/PUMA'
c               MNAME='URGENT, Air Project No 2225'
c     Please restrict this line to maximum 132 characters.
c     ------------------------------------------------------------------ 

      MNAME='Programme/Project'

c     ------------------------------------------------------------------ 
c 32. Total number of files of your data set (< 1000):
c     ------------------------------------------------------------------ 

      NVOL=2

c     ------------------------------------------------------------------ 
c 33. Number of this data file within your data set (from 1 to NVOL):
c     ------------------------------------------------------------------ 

      IVOL=1

c     ------------------------------------------------------------------ 
c 34. UT date at which the data begin.
c     For aircraft data, it is the UT date of takeoff.
c     For model data, it is the initial date of the scenario or, if this
c     does not apply, the date when the run was performed.
c     'year' must have 4 digits.
c     ------------------------------------------------------------------ 

      year=1999
      month=6
      day=18

c     ------------------------------------------------------------------ 
c 35. Date when the data were issued or revised. This date should be
c     modified whenever the data are updated.
c     'ryear' must have 4 digits.
c     ------------------------------------------------------------------ 

      ryear=2000
      rmonth=8
      rday=3

c     =========
c     INPUT END (B)
c     ==================================================================

      iu=10

      miv=NIV
      mv=NV
      mav=nav
      mauxc=NAUXC
      mscoml=NSCOML
      mncoml=NNCOML
      mimp=nimp
      mcha=ncha
      mdep=ndep
      mx1=nx1
      mx2=nx2
      mx3=nx3
      mx=maxnx

      aux=0
      if (miv.eq.1.and.mav.eq.0) aux=1

      iv2=ndep
      if (mdep.eq.1) then
        if (miv.eq.1) then
          iv2=0
          write (6,112)
        elseif (miv.eq.2) then
          if (mcha.eq.0.and.DX(1).ne.0..and.mimp.ne.0) iv2=iv2+2
        else
          write (6,113)
          write (6,114)
          write (6,118)
          stop
        endif
      endif

      iv3=1
      if (mcha.ne.0) then
        if (miv.eq.2) then
          if (mdep.ne.0) then
            iv3=6
          else
            write (6,113)
            write (6,117) miv, mdep
            write (6,118)
            stop
          endif
        else
          write (6,113)
          write (6,115) miv
          write (6,118)
          stop
        endif
      else
        if (miv.eq.1) then
          if (mav.eq.0) then
            iv3=0
          else
            if (DX(1).ne.0..and.mimp.ne.0) iv3=2
          endif
        endif
      endif

      FFI=1000*NIV+100*iv2+10*iv3+aux

      NAUXV=nav
      if (FFI.eq.2160) NAUXV=nav+mauxc

      if (mimp.ne.0) then
        if (miv.eq.2.and.mcha.ne.0) then
          write (6,120) mcha
        elseif (miv.eq.2.and.DX(1).eq.0.) then
          write (6,121)
        endif
      endif

      NX(1)=mx1
      NX(2)=mx2
      NX(3)=mx3

      do j=1,3
        do i=1,mx
          X(i,j)=0.
        enddo
      enddo

      do i=1,mx1
        if (miv.ge.2.and.DX(1).ne.0.) then
          X(i,1)=x1(1)+float(i-1)*DX(1)
        else
          X(i,1)=x1(i)
        endif
      enddo
      do i=1,mx2
        if (miv.ge.3.and.DX(2).ne.0.) then
          X(i,2)=x2(1)+float(i-1)*DX(2)
        else
          X(i,2)=x2(i)
        endif
      enddo
      do i=1,mx3
        if (miv.eq.4.and.DX(3).ne.0.) then
          X(i,3)=x3(1)+float(i-1)*DX(3)
        else
          X(i,3)=x3(i)
        endif
      enddo

      if (FFI.eq.1010.and.DX(1).ne.0.) write (6,122) DX(1)
      if (FFI.eq.2110.and.DX(1).ne.0.) write (6,123) DX(1)
      if (FFI.eq.2010.or.FFI.eq.3010.or.FFI.eq.4010) then
        iflag=0
        do i=1,ni
          if (mimp.ne.0.and.DX(i).ne.0.) then
            NXDEF(i)=1
          else
            NXDEF(i)=NX(i)
            if (mimp.eq.0.and.iflag.eq.0) then
              iflag=iflag+1
              write (6,132)
            endif
          endif
        enddo
      endif

      NLHEAD=13+miv+mv+mscoml+mncoml
      if (FFI.ne.1001) then
        NLHEAD=NLHEAD+1+NAUXV
        if (mav.ne.0) NLHEAD=NLHEAD+2
        if (FFI.eq.2160) then
          NLHEAD=NLHEAD+2+mauxc
          if (mauxc.gt.0) NLHEAD=NLHEAD+1
        elseif (FFI.eq.1020) then
          NLHEAD=NLHEAD+1
        elseif (FFI.eq.2010.or.FFI.eq.3010.or.FFI.eq.4010) then
          NLHEAD=NLHEAD+1+miv
        endif
      endif

      call lastchar (CentreID, endce)
      if (endce.eq.0) then
        write (6,106)
        stop
      endif
      call lastchar (DataID, endda)
      if (endda.eq.0) then
        write (6,107)
        stop
      endif

      yy=year-year/100*100
      write (ymd,108) yy, month, day
      filena=CentreID(1:endce)//'_'//DataID(1:endda)//'_'//ymd//'.na'
      lblank=index(filena,' ')

      write (DATE, 100) year, month, day
      write (RDATE, 100) ryear, rmonth, rday

      call lastchar (ONAME, endon)
      if (endon.eq.0) then
        write (6,102)
        stop
      endif
      call lastchar (ORG, endor)
      if (endor.eq.0) then
        write (6,103)
        stop
      endif
      call lastchar (SNAME, endsn)
      if (endsn.eq.0) then
        write (6,104)
        stop
      endif
      call lastchar (MNAME, endmn)
      if (endmn.eq.0) then
        write (6,105)
        stop
      endif
      do i=1,miv
        call lastchar (XNAME(i), endxn(i))
        if (endxn(i).eq.0) then
          write (6,109) i
          stop
        endif
      enddo
      do i=1,mv
        call lastchar (VNAME(i), endvn(i))
        if (endvn(i).eq.0) then
          write (6,111) i
          stop
        endif
      enddo
      if (mav.ne.0) then
        do i=1,mav
          call lastchar (ANAME(i), endan(i))
          if (endan(i).eq.0) then
            write (6,130) i
            stop
          endif
        enddo
      endif
      if (mauxc.ne.0) then
        do i=1,mauxc
          call lastchar (cname(i), endcn(i))
          if (endcn(i).eq.0) then
            write (6,133) i
            stop
          endif
          call lastchar (cmiss(i), endcm(i))
          if (endcm(i).eq.0) then
            write (6,134) i
            stop
          endif
        enddo
      endif

      if (lblank.eq.0) then
        open (unit=iu, file=filena, status='new', form='formatted')
      elseif (lblank.gt.13) then
        i=lblank-1
        open (unit=iu, file=filena(1:i), status='new', form='formatted')
      else
        write (6,101)
        stop
      endif

      call wrifir (iu, NLHEAD, FFI, iflag)
      if (iflag.ne.0) stop
      write (iu,200) ONAME(1:endon)
      write (iu,200) ORG(1:endor)
      write (iu,200) SNAME(1:endsn)
      write (iu,200) MNAME(1:endmn)
      call wrivol (iu, IVOL, NVOL, iflag)
      if (iflag.ne.0) stop
      write (iu,201) DATE, RDATE

      if (FFI.eq.2160) then
        write (iu,*) DX(1)
        write (iu,*) LENX
      elseif (FFI.eq.2310) then
        write (iu,*) DX(miv)
      else
        write (iu,*) (DX(i), i=1,miv)
        if (FFI.eq.1020) then
          call wrinum (iu, NVPM, 4, iflag)
          if (iflag.ne.0) then
            write (6,131) NVPM, NVPM
            stop
          endif
        elseif (FFI.eq.2010.or.FFI.eq.3010.or.FFI.eq.4010) then
          write (iu,202) (NX(j), j=1,ni)
          write (iu,202) (NXDEF(j), j=1,ni)
          do j=1,ni
            write (iu,*) (X(i,j), i=1,NXDEF(j))
          enddo
        endif
      endif

      do i=1,miv
        string=XNAME(i)
        write (iu,200) string(1:endxn(i))
      enddo
      call wrinum (iu, mv, 2, iflag)
      if (iflag.ne.0) then
        write (6,110) mv
        stop
      endif
      write (iu,*) (VSCAL(i), i=1,mv)
      write (iu,*) (VMISS(i), i=1,mv)
      do i=1,mv
        string=VNAME(i)
        write (iu,200) string(1:endvn(i))
      enddo

      if (FFI.ne.1001) then
        call wrinum (iu, NAUXV, 2, iflag)
        if (iflag.ne.0) then
          write (6,129) mav
          stop
        endif
        if (FFI.eq.2160) then
          call wrinum (iu, mauxc, 2, iflag)
        endif
        if (mav.gt.0) then
          write (iu,*) (ASCAL(i), i=1,mav)
          write (iu,*) (AMISS(i), i=1,mav)
        endif
        if (FFI.eq.2160.and.mauxc.gt.0) then
          write (iu,*) (LENA(i), i=1,mauxc)
          do i=1,mauxc
            string=cmiss(i)
            write (iu,200) string(1:endcm(i))
          enddo
        endif
        if (mav.gt.0) then
          do i=1,mav
            string=ANAME(i)
            write (iu,200) string(1:endan(i))
          enddo
        endif
        if (FFI.eq.2160.and.mauxc.gt.0) then
          do i=1,mauxc
            string=cname(i)
            write (iu,200) string(1:endcn(i))
          enddo
        endif
      endif

      call wrinum (iu, mscoml, 2, iflag)
      if (iflag.ne.0) then
        write (6,127) mscoml
        stop
      endif
      if (mscoml.gt.0) then
        do i=1,mscoml
          call lastchar (SCOM(i), j)
          string=SCOM(i)
          if (j.eq.0) write (iu,200) string(1:1)
          if (j.gt.0) write (iu,200) string(1:j)
        enddo
      endif
      call wrinum (iu, mncoml, 2, iflag)
      if (iflag.ne.0) then
        write (6,128) mncoml
        stop
      endif
      if (mncoml.gt.0) then
        do i=1,mncoml
          call lastchar (NCOM(i), j)
          string=NCOM(i)
          if (j.eq.0) write (iu,200) string(1:1)
          if (j.gt.0) write (iu,200) string(1:j)
        enddo
      endif

      close (iu)

      write (6,116) filena
      write (6,119) FFI
      if (NLHEAD.le.9) then
        write (6,124) NLHEAD
      elseif (NLHEAD.le.99) then
        write (6,125) NLHEAD
      elseif (NLHEAD.le.999) then
        write (6,126) NLHEAD
      endif

      stop

  100 format (i4,2(1x,i2.2))
  101 format (/'Sorry, too many characters in file name.'
     >     /'Please keep   L(centre-id) + L(data-id) < 30 characters.'/)
  102 format (/'Please input authors'' names (ONAME).'/)
  103 format (/'Please input organisation name (ORG).'/)
  104 format (/'Please input instrument or model name (SNAME).'/)
  105 format (/'Please input programme and project (MNAME).'/)
  106 format (/'Please input your centre identifier (CentreID).'/)
  107 format (/'Please input the data identifier (DataID).'/)
  108 format (3i2.2)
  109 format (/'Please input name of independent variable number',i2,'.'
     >        /)
  110 format (/'NV=',i5
     >        /'Are you sure you want to submit so many observations in 
     >a single file?'
     >        /'Please contact BADC: badc@rl.ac.uk.'/)
  111 format (/'Please input name of primary variable number',i2,'.'/)
  112 format (/'Warning: you said your bounded indpdt variable depended 
     >on your unbounded variable (ndep=1) but you have no bounded indpdt
     > variable.')
  113 format (/'There is no NASA Ames format suited to this case.')
  114 format ('If you have more than 2 indpdt variables,'
     >        /'they must be given on an orthogonal grid (ndep=0).')
  115 format ('If NIV=',i1,', your unbounded indpdt variable must be a r
     >eal number (ncha=0).')
  116 format (/'SUCCESSFUL!'
     >        /'Your header has been created in ',a40)
  117 format ('If NIV=',i1,' and ndep=',i1,', your unbounded indpdt vari
     >able'
     >        /'must be a real number (ncha=0).')
  118 format ('Please adapt your data format or contact BADC.'/)
  119 format ('Your FFI number is ',i4,'.')
  120 format (/'Warning: since your unbounded indpdt variable is a chara
     >cter string (ncha=',i1,'),'
     >        /'your bounded variable must be explicit.')
  121 format (/'Warning: since your bounded indpdt variable is irregular
     >ly spaced (DX(1)=0.),'
     >        /'it must be explicit.')
  122 format (/'Note that since your indpdt variable is equally spaced'
     >        /'(DX(1)=',e12.5,'),'
     >        /'you could use the more compact format 1020 (nimp=1).')
  123 format (/'Note that since the values of your bounded indpdt variab
     >le are regularly spaced (DX=',e12.5,'),'
     >        /'you could use the more compact format 2310 (nimp=1).')
  124 format ('Your file header has ',i1,' lines.'/)
  125 format ('Your file header has ',i2,' lines.'/)
  126 format ('Your file header has ',i3,' lines.'/)
  127 format (/'NSCOML=',i5/'Please keep NSCOML < 99.' /)
  128 format (/'NNCOML=',i5/'Please keep NNCOML < 99.' /)
  129 format (/'nav=',i5
     >        /'Are you sure you need so many auxiliary variables?'
     >        /'Please keep nav < 99.'/)
  130 format (/'Please input name of real auxiliary variable number',i2,
     >        '.'/)
  131 format (/'NVPM=',i5,' implies that ',i5,' values'
     >        /'of each primary variable are recorded on one line.'
     >        /'Please keep NVPM less than 4 digits long.'/)
  132 format (/'Note that since one of your bounded variables has a'
     >        /'regular interval, you could make it implicit (nimp=1).')
  133 format (/'Please input name of character string auxiliary variable
     > number',i2,'.'/)
  134 format (/'Please input missing value of character string auxiliary
     > variable number',i2,'.'/)

  200 format (a)
  201 format (a10,2x,a10)
  202 format (i6,2x,i6,2x,i6)

  300 format ('...')

      end

c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

      subroutine lastchar (s,i)

      integer       i
      character*132 s

      i=132
    1 if (s(i:i).eq.' ') then
        i=i-1
        if (i.ne.0) goto 1
      endif
      return
      end

c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

      subroutine wrinum (iu, n, maxln, iflag)

      integer iu, n, maxln, iflag

      iflag=0
      if (n.le.9) then
        write (iu,101) n
      elseif (n.le.99) then
        if (maxln.gt.1) then
          write (iu,102) n
        else
          iflag=1
        endif
      elseif (n.le.999) then
        if (maxln.gt.2) then
          write (iu,103) n
        else
          iflag=1
        endif
      elseif (n.le.9999) then
        if (maxln.gt.3) then
          write (iu,104) n
        else
          iflag=1
        endif
      elseif (n.le.99999) then
        if (maxln.gt.4) then
          write (iu,105) n
        else
          iflag=1
        endif
      else
        iflag=1
      endif
      return

  101 format (i1)
  102 format (i2)
  103 format (i3)
  104 format (i4)
  105 format (i5)

      end

c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

      subroutine wrifir (iu, m, n, iflag)

      integer iu, m, n, iflag

      iflag=0
      if (m.le.9) then
        write (iu,100) m, n
      elseif (m.le.99) then
        write (iu,101) m, n
      elseif (m.le.999) then
        write (iu,102) m, n
      else
        iflag=1
        write (6,103)
      endif
      return

  100 format (i1,2x,i4)
  101 format (i2,2x,i4)
  102 format (i3,2x,i4)
  103 format (/'Too many lines in header.'
     >        /'Please keep NLHEAD < 1000.'/)

      end

c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

      subroutine wrivol (iu, m, n, iflag)

      integer iu, m, n, iflag

      iflag=0
      if (n.ge.1000) then
        iflag=1
        write (6,100)
        return
      else
        if (m.gt.n) then
          iflag=1
          write (6,101)
          return
        else
          if (n.le.9) then
            write (iu,102) m, n
          elseif (n.le.99) then
            if (m.le.9) then
              write (iu,103) m, n
            else
              write (iu,104) m, n
            endif
          else
            if (m.le.9) then
              write (iu,105) m, n
            elseif (m.le.99) then
              write (iu,106) m, n
            else
              write (iu,107) m, n
            endif
          endif
        endif
      endif

      return

  100 format (/'Are you sure you want to submit so many files?'
     >        /'Please contact BADC: badc@rl.ac.uk.'/)
  101 format (/'NVOL is your total number of files.'
     >        /'Please keep IVOL < or = NVOL.'/)
  102 format (i1,2x,i1)
  103 format (i1,2x,i2)
  104 format (i2,2x,i2)
  105 format (i1,2x,i3)
  106 format (i2,2x,i3)
  107 format (i3,2x,i3)

      end

c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
