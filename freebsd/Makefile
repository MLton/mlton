# New ports collection makefile for:   mlton
# Date created:                30 Sep 2002
# Whom:                        Stephen Weeks <sweeks@sweeks.com>
#
# $FreeBSD$
#

PORTNAME=	mlton
PORTVERSION=	20021001 
CATEGORIES= 	lang
MASTER_SITES=	http://prdownloads.sourceforge.net/mlton/

MAINTAINER=	MLton@mlton.org
MAN1= 		mlton.1 \
		mlprof.1
MANCOMPRESSED=	yes

BUILD_DEPENDS=	\
	latex:$(PORTSDIR)/print/teTeX 				\
	latex2html:$(PORTSDIR)/textproc/latex2html		\
	mlton:$(PORTSDIR)/lang/mlton				\

LIB_DEPENDS=	\
	gmp.5:$(PORTSDIR)/math/libgmp4				\

RUN_DEPENDS= 	\

ALL_TARGET=	bootstrap
EXTRACT_SUFX=	-1.src.tgz
USE_GMAKE= 	yes
MAKE_ARGS=	DESTDIR='' PREFIX=$(PREFIX)
MANCOMPRESSED=	no

pre-build:
	gmake -C $(WRKSRC) version

.PHONY: pkg-plist
pkg-plist:
	cd $(WRKSRC)/install/usr && 				\
		find -d * \! -type d | grep -v man/man | 	\
		sort >$(.CURDIR)/pkg-plist
	cd $(WRKSRC)/install/usr && 					\
		find -d * -type d | grep mlton | sed -e 's/^/@dirrm /'	\
		>>$(.CURDIR)/pkg-plist

.include <bsd.port.mk>
