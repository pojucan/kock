C     -----------------------------------------------------------------
C     Programa Kock
C     Versao Beta: 2004
C     Autor: Marcello Pojucan Magaldi Santos e Raul Oscar Vallejos
C     Licenca:
C     O usu†rio pode usar desde que n∆o seja para fins lucrativos. Pode
C     alterar o c¢digo fonte, desde que comunique previamente o autor.
C     -----------------------------------------------------------------
      Program KOCK
      implicit none
c
      integer j,k,N,p,s,z,dim
      parameter (dim=10000000)
c
      real xold(dim),xnew(dim)
      real yold(dim),ynew(dim)
c
      real x1,x2,x3,x4,x5
      real y1,y2,y3,y4,y5
      real xleft,yleft,xright,yright
c
      write (*,*) '--------------------------------------------------'
      write (*,*) '  KOCK                                            '
      write (*,*) '--------------------------------------------------'
c
      write (*,*)
      write (*,*) ' Digite abaixo a profundidade da curva:'
      write (*,*)
      read (*,*) N
      data xleft/0.0/,yleft/0.0/,xright/3.0/,yright/0.0/
c
      x1=xleft
      y1=yleft
      x5=xright
      y5=yright
c
      call tijolokoch(x1,y1,x5,y5,x2,y2,x3,y3,x4,y4)
c
      xnew(1)=x1
      xnew(2)=x2
      xnew(3)=x3
      xnew(4)=x4
      xnew(5)=x5
c
      ynew(1)=y1
      ynew(2)=y2
      ynew(3)=y3
      ynew(4)=y4
      ynew(5)=y5
c
      do j=1,5
      xold(j)=xnew(j)
      yold(j)=ynew(j)
      end do
c
      p=(4**N)+1
c
      do z=2,N
c
c
      s=4**N
c
      do k=1,s
      x1=xold(k)
      x5=xold(k+1)
      y1=yold(k)
      y5=yold(k+1)
c
      call tijolokoch(x1,y1,x5,y5,x2,y2,x3,y3,x4,y4)
c
      j=1+(4*(k-1))
c
      xnew(j)=x1
      xnew(j+1)=x2
      xnew(j+2)=x3
      xnew(j+3)=x4
      xnew(j+4)=x5
c
      ynew(j)=y1
      ynew(j+1)=y2
      ynew(j+2)=y3
      ynew(j+3)=y4
      ynew(j+4)=y5
c
      end do
c
      do k=1,s
      xold(k)=xnew(k)
      yold(k)=ynew(k)
      end do
c
      end do
c
      write (*,*)
      write (*,*) 'Temos abaixo a tabela encontrada'
      write (*,*)
c
      do k=1,p
      write (*,*) xnew(k),ynew(k)
      write (55,*) xnew(k),ynew(k)
      end do
c
      write (*,*) 'n£mero de pontos gerados',p
      write (*,*) '-----------------------------------------'
      write (*,*) 'Esse programa gera a tabela em um bloco de'
      write (*,*) 'notas para ser impresso ou ser usado posteriormente'
      write (*,*) 'em um programa gr†fico. O arquivo Ç fort.55'
      read (*,*)
      stop
      end
c
c     SUB-ROTINA
c
      subroutine tijolokoch(x1,y1,x5,y5,x2,y2,x3,y3,x4,y4)
c
      implicit none
c
      real x1,x2,x3,x4,x5
      real y1,y2,y3,y4,y5
c
      real pi,fi
c
      pi=asin(1.0)*2.0
      fi=(pi/3.0)
c
      x2=x1+(x5-x1)/3.0
      y2=y1+(y5-y1)/3.0
c
      x4=x1+(2.0*(x5-x1)/3.0)
      y4=y1+(2.0*(y5-y1)/3.0)
c
      x3=x2+cos(fi)*(x4-x2)-sin(fi)*(y4-y2)
      y3=y2+sin(fi)*(x4-x2)+cos(fi)*(y4-y2)
c
      return
      end
