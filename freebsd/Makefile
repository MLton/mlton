# New ports collection makefile for:   mlton
# Date created:                1 Oct 2002
# Whom:                        Stephen Weeks <sweeks@sweeks.com>
#
# $FreeBSD$
#

# Because MLton is written in SML, it needs an SML compiler to build.
# It is easiest to use another (earlier) binary version of MLton as
# the SML compiler.  So, this Makefile fetches and installs an
# alread-built version of MLton, BOOT_DIST, to use for bootstrapping.

PORTNAME=	mlton
PORTVERSION=	MLTONVERSION
CATEGORIES=	lang
MASTER_SITES=	http://www.mlton.org/download/	\
		http://www.mlton.org/experimental/
DISTFILES=	${BOOT_DIST} ${SRC_DIST}
EXTRACT_ONLY=	${SRC_DIST}

MAINTAINER=	MLton@mlton.org
COMMENT=	An optimizing Standard ML compiler

BUILD_DEPENDS=	hevea:${PORTSDIR}/textproc/hevea: 	\
		latex:${PORTSDIR}/print/teTeX:
LIB_DEPENDS=	gmp.6:${PORTSDIR}/math/libgmp4

SRC_DIST=	${DISTNAME}-1.freebsd.src.tgz
BOOT_DIST=	${PORTNAME}-20040623-1.i386-freebsd.tbz

ONLY_FOR_ARCHS=	i386

MAN1=		mllex.1 mlprof.1 mlton.1 mlyacc.1
MANCOMPRESSED=	yes

BOOT_WRKSRC=	${WRKDIR}/mlton-bootstrap

USE_REINPLACE=	yes
ALL_TARGET=	all
USE_GMAKE=	yes
MAKE_ARGS=	DESTDIR='' \
		PREFIX=${PREFIX}

PORTDIR=	usr/ports/lang/mlton

.PHONY: post-extract
post-extract:
	@${MKDIR} ${BOOT_WRKSRC}
	@${TAR} xf ${DISTDIR}/${BOOT_DIST} --bzip -C ${BOOT_WRKSRC}
	@${REINPLACE_CMD} -e \
		"s|lib=\'${LOCALBASE}/|lib=\'${BOOT_WRKSRC}/|" \
	${BOOT_WRKSRC}/bin/mlton

# Stuff below this line is for maintainer use only.

.PHONY: build-package
build-package:
	export PATH=${BOOT_WRKSRC}/bin:${PATH} && ${MAKE} build
	${MAKE} pkg-plist
	${MAKE} deinstall
	${MAKE} install
	${MAKE} package
	${MAKE} deinstall
	pkg_add ${PORTNAME}-${PORTVERSION}.tbz
	${MAKE} deinstall
	${MAKE} reinstall
	${MAKE} package
	cp ${PORTNAME}-${PORTVERSION}.tbz	\
		${DISTDIR}/${PORTNAME}-${PORTVERSION}-1.i386-freebsd.tbz
	${MAKE} makesum
	portlint .
	${TAR} -cpf - Makefile distinfo pkg-descr pkg-plist | \
		( ${MKDIR} ${PORTDIR} && cd ${PORTDIR} && ${TAR} -xpf - )
	shar `find ${PORTDIR}` >/tmp/mlton-${PORTVERSION}-portdir.shar

TMP_PLIST=	/tmp/mlton.pkg-plist
PLIST=		${.CURDIR}/pkg-plist

pkg-plist:
	cd ${WRKSRC} && ${GMAKE} install
	cd ${WRKSRC}/install/usr && \
		${FIND} -d * \! -type d | ${GREP} -v man/man | \
		sort >${PLIST}
	cd ${WRKSRC}/install/usr && \
		${FIND} -d * -type d | ${GREP} mlton | \
			${SED} -e 's/^/@dirrm /' >>${PLIST}
	${SED} -e 's;@dirrm share/doc/mlton;%%PORTDOCS%%@dirrm %%DOCSDIR%%;' \
		<${PLIST} >${TMP_PLIST}
	${SED} -e 's;share/doc/mlton;%%PORTDOCS%%%%DOCSDIR%%;'	\
		<${TMP_PLIST} >${PLIST}

.include <bsd.port.mk>
