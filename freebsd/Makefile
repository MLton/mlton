# New ports collection makefile for:   mlton
# Date created:                1 Oct 2002
# Whom:                        Stephen Weeks <sweeks@sweeks.com>
#
# $FreeBSD$
#

PORTNAME=	mlton
PORTVERSION=	MLTONVERSION
CATEGORIES= 	lang
MASTER_SITES=					\
	http://www.mlton.org/download/		\
	http://www.mlton.org/experimental/
DISTFILES=		\
	${BOOT_DIST}	\
	${SRC_DIST}
EXTRACT_ONLY=	${SRC_DIST}

MAINTAINER=	MLton@mlton.org
COMMENT=	An optimizing Standard ML compiler

# MLton build depends on itself.  There is no easy way to avoid this.
# The code below is supposed to instal an old version of MLton,
# version 20040429, that runs on FreeBSD 4.X and is statically linked
# against the GnuMP, so that all it needs is the compat4x package in
# order to run on a FreeBSD 5.X machine.
BUILD_DEPENDS= 					\
	hevea:${PORTSDIR}/textproc/hevea:	\
	latex:${PORTSDIR}/print/teTeX: 		\
	mlton:${PORTSDIR}/lang/mlton:
LIB_DEPENDS= 					\
	gmp.6:${PORTSDIR}/math/libgmp4

SRC_DIST=	${DISTNAME}-1.src.tgz
BOOT_DIST=	mlton-20040429-1.i386-freebsd.tgz

ONLY_FOR_ARCHS=	i386

MAN1=			\
	mllex.1 	\
	mlprof.1 	\
	mlton.1 	\
	mlyacc.1
MANCOMPRESSED=	yes

BOOT_WRKSRC=	${WRKDIR}/mlton-bootstrap

USE_REINPLACE=	yes
ALL_TARGET=	all
USE_GMAKE= 	yes
MAKE_ARGS=	DESTDIR='' 	\
		PREFIX=${PREFIX}

PORTDIR=	usr/ports/lang/mlton

.PHONY: build-package
build-package:
	export PATH=${BOOT_WRKSRC}/bin:${PATH} && ${MAKE} makesum build
	portlint .
	${MAKE} deinstall
	${MAKE} install
	${MAKE} package
	tar -cpf - Makefile distinfo pkg-descr pkg-plist | \
		( ${MKDIR} ${PORTDIR} && cd ${PORTDIR} && tar -xpf - )
	shar `find ${PORTDIR}` >/tmp/mlton-${PORTVERSION}-portdir.shar

.PHONY: pkg-plist
pkg-plist:
	cd ${WRKSRC} && ${GMAKE} install
	cd ${WRKSRC}/install/usr && 				\
		${FIND} -d * \! -type d | ${GREP} -v man/man | 	\
		sort >${.CURDIR}/pkg-plist
	cd ${WRKSRC}/install/usr && 					\
		${FIND} -d * -type d | ${GREP} mlton | ${SED} -e 's/^/@dirrm /'	\
		>>${.CURDIR}/pkg-plist

.PHONY: post-build
post-build:
	${MAKE} pkg-plist

.PHONY: post-extract
post-extract:
	@${MKDIR} ${BOOT_WRKSRC}
	@${TAR} zxf ${DISTDIR}/${BOOT_DIST} -C ${BOOT_WRKSRC}
	@${REINPLACE_CMD} -e \
		"s|lib=\'${LOCALBASE}/|lib=\'${BOOT_WRKSRC}/|" \
	${BOOT_WRKSRC}/bin/mlton

.include <bsd.port.mk>

#.include <bsd.port.pre.mk>
#.include <bsd.port.post.mk>
