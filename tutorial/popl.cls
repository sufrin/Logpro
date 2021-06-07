\ProvidesClass{popl}[1996/04/17 Bernard Sufrin's POPL Foil Article Class]
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{foil}}
\ProcessOptions
\LoadClass{foil}
\RequirePackage{euler}
\RequirePackage{alltt}
%\RequirePackage{amscd} % Just for the commutative diagrams
%\RequirePackage{amsmath}% Just for the commutative diagrams code
\RequirePackage{abbreviations}
\RequirePackage{dinglist}
\RequirePackage{xspace}
\RequirePackage{exercises}
\RequirePackage{RCS}
%\RequirePackage[dvips]{color}

\AtBeginDocument{\let\notesize=\normalsize
\let\answersize=\normalsize
\let\JavaSize=\normalsize
}


%%%%%%%%%%%%%%%%%%%%%%%%
%\newenvironment{framed}[1][4in]{%
%\begin{center}
%\begin{tabular}{|c|}\hline\\
%\begin{minipage}{#1}}{%
%\end{minipage}\\\\\hline
%\end{tabular}
%\end{center}}
%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
\usepackage{shadow}
%
%
%%%%%%%%%%%%%%%%%%%%%%%

% Syntehtic symbols
\def\pfun{\mathbin{->\!\!\!\!\!\rule[.20ex]{.5pt}{.75ex}~~}}
\newcommand{\imgbra}{\mathopen{(\!\rule[-.25ex]{.5pt}{1.75ex}\;}}
\newcommand{\imgket}{\mathclose{)\!\!\rule[-.25ex]{.5pt}{1.75ex}\;\;}}
\def\env{\mathbin{\rho}}
\let\kon=\kappa
\def\llongrightarrow{\stackrel{L}{\longrightarrow}}
\def\clongrightarrow{\stackrel{C}{\longrightarrow}}

% Numbered lists
\newenvironment{Enumerate}{\begin{dingautolist}{192}}{\end{dingautolist}}

% Sidebars for notes
\AtBeginDocument{\def\SIDEBAR{{\ding{229}}}}

% Stuff
\newenvironment{turn}[1]{}{}
\def\IPL#1{{\sc PLain}}
\def\cpm{$C_\pm$}
% Programs
\def\dom{\mathop{\mathbf{dom}}}
\def\rng{\mathop{\mathbf{range}}}
\def\ran{\mathop{\mathbf{range}}}


\def\Isnt{\mathrel{\mathbf{isn't}}}
\def\Skip{\mathord{\mathbf{skip}}}
\def\True{\mathord{\mathbf{true}}}
\def\False{\mathord{\mathbf{false}}}
\def\While{\mathop{\mathbf{while}}}
\def\Do{\mathopen{\mathbf{do~}}}
\def\If{\mathopen{\mathbf{if~}}}
\def\After{\mathbin{\mathbf{after}}}
\def\Before{\mathbin{\mathbf{before}}}
\def\Then{\mathbin{\mathbf{then}}}
\def\Else{\mathbin{\mathbf{else}}}
\def\Fi{\mathclose{\mathbf{~fi}}}
\def\Od{\mathclose{\mathbf{~od}}}
\def\End{\mathclose{\mathbf{~end}}}
\def\Case{\mathopen{\mathbf{case~}}}
\def\Of{\mathbin{\mathbf{of}}}
\def\Con{\mathop{\mathbf{const}}}
\def\Var{\mathop{\mathbf{var}}}
\def\Where{\mathop{\mathbf{where}}}
\def\Within{\mathop{\mathbf{within}}}
\def\Proc{\mathop{\mathbf{proc}}}
\def\Fun{\mathop{\mathbf{fun}}}
\def\Val{\mathop{\mathbf{val}}}
\def\Res{\mathop{\mathbf{res}}}
\def\Valres{\mathop{\mathbf{valres}}}
\def\Ref{\mathop{\mathbf{ref}}}
\def\Switch{\mathop{\mathbf{switch}}}
\def\And{\mathrel{\mathbf{and}}}
\def\Type{\mathop{\mathbf{type}}}
\def\Record{\mathop{\mathbf{record}}}
\def\Array{\mathop{\mathbf{array}}}
\def\New{\mathop{\mathbf{new}}}
\def\Module{\mathop{\mathbf{module}}}
\def\Set{\mathop{\mathbf{set}}}

\def\fatcolon{\mathrel{\aboveset{\circ}{\circ}}}
\def\fatcolonel{\mathrel{\aboveset{\bullet}{\circ}}}

% Notation
\let\bottom=\perp
\def\reduces{\mathrel{-\!\!\!-\!\!\triangleright}}
\def\reducesplus{\stackrel{+}{\reduces}}
\def\reducesstar{\stackrel{\ast}{\reduces}}
\newcommand{\reaches}[1][\relax]{{@> ~~#1^{+}~~ >>}}
\newcommand{\Reaches}[1][\relax]{{@> ~~#1^\ast~~ >>}}
\newcommand{\nf}{\surd} % I'd prefer a real tick...
\renewcommand\reaches[1][]{\stackrel{#1^{+}}{\longrightarrow}}
\renewcommand\Reaches[1][]{\stackrel{#1^{\ast}}{\longrightarrow}}

% Borrowed from AMSTeX
\def\setboxz@h{\setbox\z@\hbox}

\def\wdz@{\wd\z@}

\def\binrel@#1{\begingroup
  \setboxz@h{\thinmuskip0mu
    \medmuskip\m@ne mu\thickmuskip\@ne mu
    \setbox\tw@\hbox{$#1\m@th$}\kern-\wd\tw@
    ${}#1{}\m@th$}%
  \edef\@tempa{\endgroup\let\noexpand\binrel@@
    \ifdim\wdz@<\z@ \mathbin
    \else\ifdim\wdz@>\z@ \mathrel
    \else \relax\fi\fi}%
  \@tempa
}

\def\overset#1#2{\binrel@{#2}%
  \binrel@@{\mathop{\kern\z@#2}\limits^{#1}}}
\def\underset#1#2{\binrel@{#2}%
  \binrel@@{\mathop{\kern\z@#2}\limits_{#1}}}

\def\aboveset#1#2{\binrel@{_#2}%
  \binrel@@{\mathop{\kern\z@_#2}\limits^{#1}}}



% Mathematical Bits %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{formal}{\begin{array}{*{20}{l}}}{\end{array}}
\def\[{
\ifthenelse{\boolean{mmode}}{\def\@changemode{\relax}\def\@closebra{\right]}\left[}%
{\def\@changemode{$$}\let\@closebra=\relax}\@changemode\begin{formal}}
\def\]{\end{formal}\@closebra\@changemode}

\let\beginmath=\(
\let\endmath=\)

\def\({\ifthenelse{\boolean{mmode}}{\let\@changemode=\relax\def\@closebra{\end{formal}\right)}\left(\begin{formal}}{\let\@changemode=\beginmath\let\@closebra=\endmath}\@changemode}
\def\){\@closebra}

\def\Cases{\left\lbrace\begin{formal}}
\def\OpenBrace{\left\lbrace\begin{formal}}
\def\CloseBrace{\end{formal}\right\rbrace}

%\let\@CurlyOpen=\{
%\let\@CurlyClose=\}
%\def\{{\ifthenelse{\boolean{mmode}}{\OpenBrace}{\@CurlyOpen}}
%\def\}{\ifthenelse{\boolean{mmode}}{\CloseBrace}{\@CurlyClose}}

\def\<{\left\langle\begin{formal}}
\def\>{\end{formal}\right\rangle}

\def\.{\end{formal}\right.}

\let\@lambda=\lambda
\def\lambda{\ifthenelse{\boolean{mmode}}{\mathop{\@lambda}}{\beginmath\@lambda\endmath}}

\let\@cdot=\cdot
\def\cdot{\ifthenelse{\boolean{mmode}}{\mathbin{\@cdot}}{\beginmath\@cdot\endmath}}


\let\semanticop=\overline
% Inference Rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\newenvironment{inference}[1][\relax]{%
        \def\@therulename{#1}
        \smaller
        \ifthenelse{\boolean{mmode}}
                   {\def\@changemode{\relax}}
                   {\def\@changemode{$$}}
        \@changemode
        \setlength{\arraycolsep}{1pt}
        \begin{array}{llllllr}
}{\end{array}\@changemode}


\newcommand{\infer}[1][\@therulename]{
\\[-0.95ex]
\hrulefill&\hrulefill&\hrulefill&\hrulefill&\hrulefill&\lower0.5ex\hbox{\Smaller#1}
\\\relax}


\newcommand{\Hline}{\\\hline}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

















































