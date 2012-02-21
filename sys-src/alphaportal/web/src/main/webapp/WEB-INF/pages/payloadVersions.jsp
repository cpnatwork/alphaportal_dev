<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="payloadVersions.title"/></title>
</head>


<c:if test="${not empty isErrors}">
	<input type="button" class="button" onclick="location.href='<c:url value="caseMenu"/>'" value="<fmt:message key="button.backToMainMenu"/>"/>
</c:if>

<c:if test="${empty isErrors}">
	<h1>${cardName}</h1>
	
	<display:table name="payloadList" class="table" requestURI="" id="payloadList">
	    <display:column property="payloadIdentifier.sequenceNumber" titleKey="payloadVersions.id"/>
	    <display:column property="filename" href="" media="html" paramId="seqNumber" paramProperty="payloadIdentifier.sequenceNumber" titleKey="payloadVersions.name"/>
	
	    <display:setProperty name="paging.banner.item_name"><fmt:message key="caseList.case"/></display:setProperty>
	    <display:setProperty name="paging.banner.items_name"><fmt:message key="caseList.cases"/></display:setProperty>
	    
	</display:table>
	
	<input type="button" class="button" onclick="location.href='<c:url value="caseform?caseId=${caseId}&activeCardId=${cardId}"/>'" value="<fmt:message key="button.cancel"/>"/>
</c:if>
