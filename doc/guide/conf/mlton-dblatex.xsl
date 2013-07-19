<?xml version="1.0" encoding="iso-8859-1"?>
<!--
dblatex(1) XSL user stylesheet for asciidoc(1).
See dblatex(1) -p option.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:param name="doc.collab.show">0</xsl:param>
  <xsl:param name="doc.lot.show"></xsl:param>
  <xsl:param name="doc.publisher.show">0</xsl:param>
  <xsl:param name="doc.section.depth">3</xsl:param>
  <xsl:param name="doc.toc.show">1</xsl:param>
  <!-- TOC links in the titles, and in blue. -->
  <xsl:param name="latex.hyperparam">colorlinks,linkcolor=blue</xsl:param>
  <xsl:param name="latex.output.revhistory">0</xsl:param>
  <xsl:param name="monoseq.hyphenation">nohyphen</xsl:param>
  <xsl:param name="table.in.float">0</xsl:param>
  <xsl:param name="term.breakline">1</xsl:param>
  <xsl:param name="toc.section.depth">1</xsl:param>

  <xsl:template match="processing-instruction('asciidoc-pagebreak')">
    <xsl:text>\pagebreak[4]&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="processing-instruction('asciidoc-br')">
    <xsl:text>\newline&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="processing-instruction('asciidoc-hr')">
    <!-- draw a 444 pt line (centered) -->
    <xsl:text>\begin{center}&#10; </xsl:text>
    <xsl:text>\line(1,0){444}&#10; </xsl:text>
    <xsl:text>\end{center}&#10; </xsl:text>
  </xsl:template>

</xsl:stylesheet>
