program simplex(input, output); 

{ two-phase simplex algorithm: version Feb. 24, 1988 }

{ copyright K. Steiglitz }
{ Computer Science Dept. }
{ Princeton University 08544 }
{ ken@princeton.edu }

const
    maxpivots = 1000;   { maximum no. of pivots }
    large = 1.0e+31;    { large number used in search for minimum cost column }
    lowlim = -1.0e+31;  { large negative number to test for unboundedness }
    mmax = 32;          { max. no. of rows }
    ncolmax = 50;       { max. no. of columns allowed in tableau }
    eps = 1.0e-8;       { for testing for zero }

var
    done, unbounded, optimal: boolean;          { flags for simplex }
    result: (toomanycols, unbound, infeas, toomanypivots, opt);
    m: 1..mmax;         { no. of rows - 1, the rows are numbered 0..m }
    numpivots: integer; { pivot count }
    pivotcol, pivotrow: integer;        { pivot column and row }
    pivotel: real;                      { pivot element }
    cbar: real;         { price when searching for entering column }
    carry: array[-1..mmax, -1..mmax] of real;   { inverse-basis matrix of the
						  revised simplex method }
    phase: 1..2;        { phase }
    price: array[0..mmax] of real;      { shadow prices = row -1 of carry =
					  -dual variables }
    basis: array[0..mmax] of integer;  { basis columns, negative integers
					  artificial }
    ncol: 1..ncolmax;                   { number of columns }
    tab: array[0..mmax, 1..ncolmax] of real;    { tableau }
    lhs: array[0..mmax] of real;        { left-hand-side }
    d: array[1..ncolmax] of real;       { current cost vector }
    c: array[1..ncolmax] of real;       { cost vector in original problem }
    curcol: array[-1..mmax] of real;    { current column }
    curcost: real;                      { current cost }
    i, col, row: integer;               { miscellaneous variables }

procedure columnsearch;
{ looks for favorable column to enter basis.
  returns lowest cost and its column number, or turns on the flag optimal }

var
  i , col : integer;
  tempcost: real;           { minimum cost, temporary cost of column }

  begin  { columnsearch }
    for i:= 0 to m do price[i]:= -carry[-1, i];  { set up price vector }
    optimal:= false;
    cbar:= large;
    pivotcol:= 0;
    for col:= 1 to ncol do
      begin
	tempcost:= d[col];
	for i:= 0 to m do tempcost:= tempcost - price[i]*tab[i, col];
	if( cbar > tempcost ) then
	  begin
	   cbar:= tempcost;
	   pivotcol:= col
	  end
      end;  { for col }
    if ( cbar > -eps ) then optimal:= true
  end;   { columnsearch }


procedure rowsearch;
{  looks for pivot row. returns pivot row number,
   or turns on the flag unbounded }

var
  i, j: integer;
  ratio, minratio: real;        { ratio and minimum ratio for ratio test }

  begin  { rowsearch }
    for i:= 0 to m do           { generate column }
      begin
	curcol[i]:= 0.0;        { current column = B inverse * original col. }
	for  j:= 0 to m do curcol[i]:=
			   curcol[i] + carry[i, j]*tab[j, pivotcol]
      end;
  curcol[-1]:= cbar;            { first element in current column }
  pivotrow:= -1;
  minratio:= large;
  for i:= 0 to m do                             { ratio test }
    begin
      if( curcol[i] > eps ) then
	begin
	  ratio:= carry[i, -1]/curcol[i];
	    if( minratio > ratio ) then         { favorable row }
	      begin
		minratio:= ratio;
		pivotrow:= i;
		pivotel:= curcol[i]
	      end
	    else { break tie with max pivot }
	      if ( (minratio = ratio) and (pivotel < curcol[i]) ) then
		  begin
		    pivotrow:= i;
		    pivotel:= curcol[i]
		  end
	end  { curcol > eps }
      end;  { for i }
    if ( pivotrow = -1 ) then  unbounded:= true         { nothing found }
			 else  unbounded:= false
  end;  { rowsearch }


procedure pivot;
{ pivots }

  var
    i, j: integer;

  begin { pivot }
    basis[pivotrow]:= pivotcol;
    for j:= -1 to m do carry[pivotrow, j]:= carry[pivotrow, j]/pivotel;
    for i:= -1 to m do
      if( i<> pivotrow ) then
	for j:= -1 to m do
	  carry[i, j]:= carry[i, j] - carry[pivotrow, j]*curcol[i];
    curcost:= -carry[-1, -1]
  end;  { pivot }


procedure changephase;
{ changes phase from 1 to 2, by switching to original cost vector }

  var
    i, j, b: integer;

  begin  { changephase }
    phase:= 2;
    for i:= 0 to m do if( basis[i] <= 0 ) then
      writeln( '...artificial basis element ', basis[i]:5,
	       ' remains in basis after phase 1');
    for j:= 1 to ncol do d[j]:= c[j];   { switch to original cost vector }
    for j:= -1 to m do
      begin
	carry[-1, j]:= 0.0;
	for i:= 0 to m do
	  begin
	    b:= basis[i];       { ignore artificial basis elements that are }
	    if( b >= 1 ) then   { still in basis }
	      carry[-1, j]:= carry[-1, j] - c[b]*carry[i,j]
	  end  { for i }
      end;  { for j }
    curcost:= -carry[-1, -1]
  end;   { changephase }

procedure setup;
{ sets up test problem, lhs = tab*x, x >= 0, min c*x }
{ nrow = number of rows; ncol = number of cols }
{ tab = tableau; lhs = constants }

var
  i, j, nrow: integer;

begin { setup }
  readln(nrow);         { read number of rows }
  readln(ncol);         { read number of columns }
  m:= nrow - 1;         { rows are numbered 0..m }
  for j:= 1 to ncol do
   read(c[j]);          { cost vector }
  for i:= 0 to m do
   begin
    read(lhs[i]);       { left-hand-side }
    for j:= 1 to ncol do
     read(tab[i, j])    { tableau }
   end;

  done:= false;                      { initialize carry matrix, etc. }
  phase:= 1;
  for i:= -1 to m do for j:= -1 to mmax do carry[i, j]:= 0.0;
  for i:= 0 to m do carry[i, i]:= 1.0;            { artificial basis }
  for i:= 0 to m do
    begin
      carry[i, -1]:= lhs[i];      { -1 col of carry = left-hand-side }
      carry[-1, -1]:= carry[-1, -1] - lhs[i]        { - initial cost }
    end;
  curcost:= -carry[-1, -1];
  for i:= 0 to m do basis[i]:= -i;       { initial, artificial basis }
  if( ncol <= ncolmax ) then               { check number of columns }
    for col:= 1 to ncol do      { initialize cost vector for phase 1 }
      begin
	d[col]:= 0.0;
	for row:= 0 to m do d[col]:= d[col] - tab[row, col]
      end
  else
    begin
      writeln('...termination: too many columns for storage');
      done:= true;
      result:= toomanycols
    end;
  numpivots:= 0;
end; { setup }


begin  { simplex }
  setup;
  while( (numpivots < maxpivots) and (not done) and
	 ( (curcost > lowlim) or (phase = 1) ) ) do
    begin
      columnsearch;
      if( not optimal ) then
	begin                         { not optimal }
	  rowsearch;
	    if( unbounded ) then
	      begin
		done:= true;
		result:= unbound;
		writeln('problem is unbounded')
	      end
	    else
	      begin
		pivot;
		numpivots:= numpivots + 1;
		if ( (numpivots = 1 ) or ( numpivots mod 10 = 0 ) ) then
		      writeln('pivot ', numpivots:4, ' cost= ', curcost:12)
	      end
	end  { not optimal }
      else                            { optimal }
	  if( phase = 1 ) then
	    begin
	      if( curcost > eps ) then
		begin
		  done:= true;
		  result:= infeas;
		  writeln('problem is infeasible')
		end
	      else
		begin
		  if ( (numpivots <> 1 ) and ( numpivots mod 10 <> 0 ) ) then
		    writeln('pivot ', numpivots:4, ' cost= ', curcost:12);
		  writeln('phase 1 successfully completed');
		  changephase
		end
	    end  { if phase = 1 }
	  else
	    begin
	      if ( (numpivots <> 1 ) and ( numpivots mod 10 <> 0 ) ) then
		writeln('pivot ', numpivots:4, ' cost= ', curcost:12);
	      writeln('phase 2 successfully completed');
	      done:= true;
	      result:= opt
	    end
    end;  { while }
  if( (curcost <= lowlim) and (phase = 2) ) then
    begin
      if ( (numpivots <> 1 ) and ( numpivots mod 10 <> 0 ) ) then
	writeln('pivot ', numpivots:4, ' cost= ', curcost:12);
      result:= unbound;
      writeln('problem is unbounded')
    end;
  if ( numpivots >= maxpivots ) then
    begin
      writeln('...termination: maximum number of pivots exceeded');
      result:= toomanypivots
    end;

  if result = opt then
    begin
      writeln('optimal solution reached');
      writeln('cost    =', -carry[-1,-1]:10:6);
      for i:= 0 to m do
	writeln('x(', basis[i]:4, ')= ', carry[i,-1]:10:6)
    end

end.

