<%@ include file="/common/taglibs.jsp"%>

<% 
	Object loc = session.getAttribute("org.apache.struts2.action.LOCALE");
	String query = StringEscapeUtils.unescapeHtml(request.getQueryString());
	if(query == null) query = "";
	if(loc != null) {
		pageContext.setAttribute("currentLocale", loc.toString()); 
		pageContext.setAttribute("currentURL", request.getRequestURL() + "?" + query.replaceAll("&locale=[^&]+", ""));
	}
	else {
	    java.util.Locale locale = request.getLocale();
	    if(locale == null) locale = java.util.Locale.ENGLISH;
	    pageContext.setAttribute("currentLocale", locale.getLanguage() ); 
		pageContext.setAttribute("currentURL", request.getRequestURL() + "?" + query.replaceAll("&locale=[^&]+", ""));
	}
%>

<%@page import="org.apache.commons.lang.StringEscapeUtils"%><c:choose>
	<c:when test="${currentLocale ne 'en'}">
		<div id="switchLocale"><a href="<c:out value="${currentURL}" escapeXml="false"/>&locale=en"><fmt:message key="webapp.name"/> in English</a></div>
	</c:when>
	<c:otherwise>
		<div id="switchLocale"><a href="<c:out value="${currentURL}" escapeXml="false"/>&locale=de"><fmt:message key="webapp.name"/> in Deutsch</a></div>
	</c:otherwise>	
</c:choose>

<div id="branding">
    <h1><a href="<c:url value='/'/>"><fmt:message key="webapp.name"/></a></h1>
    <p><fmt:message key="webapp.tagline"/></p>
</div>
<hr />

<%-- Put constants into request scope --%>
<appfuse:constants scope="request"/>