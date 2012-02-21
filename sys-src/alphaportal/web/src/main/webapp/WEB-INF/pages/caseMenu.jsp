<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="caseMenu.title"/></title>
</head>

<display:table name="caseList" class="table" requestURI="" id="caseList">
    <display:column property="caseId" sortable="true" href="caseform?isMyWorklist=1" media="html" paramId="caseId" paramProperty="caseId" titleKey="case.id"/>
    <display:column property="name" sortable="true" titleKey="case.name"/>

    <display:setProperty name="paging.banner.item_name"><fmt:message key="caseList.case"/></display:setProperty>
    <display:setProperty name="paging.banner.items_name"><fmt:message key="caseList.cases"/></display:setProperty>
    
</display:table>

<input type="button" class="button" style="margin-right: 5px" onclick="location.href='<c:url value="caseform"/>'" value="<fmt:message key="button.add"/>"/>
<input type="button" class="button" onclick="location.href='<c:url value="mainMenu"/>'" value="<fmt:message key="button.backToMainMenu"/>"/>
<input type="button" class="button" onclick="location.href='<c:url value="dashBoard"/>'" value="<fmt:message key="mainMenu.dashboard"/>"/>