# New ports collection makefile for:   mlton
# Date created:                17 Oct 2002
# Whom:                        Stephen Weeks <sweeks@sweeks.com>
#
# $FreeBSD$
#

PORTNAME=	mlton
PORTVERSION=	VERSION
CATEGORIES= 	lang
MASTER_SITES=	${MASTER_SITE_SOURCEFORGE}
EXTRACT_SUFX=	-1.src.tgz

MAINTAINER=	MLton@mlton.org

# MLton build depends on itself.  There is no easy way to avoid this.  So, in
# order to build MLton, you must have installed a binary MLton package, which
# you can get from www.mlton.org.
BUILD_DEPENDS= \
	latex:$(PORTSDIR)/print/teTeX: 				\
	latex2html:$(PORTSDIR)/textproc/latex2html:		\
	mlton:$(PORTSDIR)/lang/mlton:
LIB_DEPENDS= \
	gmp.5:$(PORTSDIR)/math/libgmp4

MAN1= 		mlton.1 \
		mlprof.1

MANCOMPRESSED=	no

ALL_TARGET=	bootstrap
USE_GMAKE= 	yes
MAKE_ARGS=	DESTDIR='' PREFIX=$(PREFIX)

.PHONY: build-package
build-package:
	$(MAKE) makesum build
	portlint .
	$(MAKE) deinstall
	$(MAKE) install
	$(MAKE) package
	$(MAKE) deinstall
	pkg_add $(PACKAGES)/All/$(DISTNAME).tgz
	$(MAKE) deinstall
	$(MAKE) reinstall
	$(MAKE) package

.PHONY: pkg-plist
pkg-plist:
	cd $(WRKSRC) && $(GMAKE) install
	cd $(WRKSRC)/install/usr && 				\
		find -d * \! -type d | $(GREP) -v man/man | 	\
		sort >$(.CURDIR)/pkg-plist
	cd $(WRKSRC)/install/usr && 					\
		find -d * -type d | $(GREP) mlton | $(SED) -e 's/^/@dirrm /'	\
		>>$(.CURDIR)/pkg-plist

.PHONY: post-build
post-build:
	$(MAKE) pkg-plist

.include <bsd.port.mk>
