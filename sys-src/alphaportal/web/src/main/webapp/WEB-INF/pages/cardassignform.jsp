<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="addUser.title"/></title>
    <meta name="heading" content="<fmt:message key='addUser.heading'/>"/>
</head>

<form method="post">
   	<display:table name="users" class="table" requestURI="" id="row">
    	<display:column>
		    <input type="radio" name="user" value="<c:out value="${row.userId}"/>" />
		</display:column>
    	
   		<display:column property="user.lastName" media="html" titleKey="user.lastName"/>
   		<display:column property="user.firstName" media="html" titleKey="user.firstName"/>
  	</display:table>
  	<c:if test="${not empty users}">
  		<input type="submit" name="assign" value="<fmt:message key="button.assignOther"/>"/>
	</c:if>
	<input type="submit" name="cancel" value="<fmt:message key="button.cancel"/>" onclick="bCancel=true"/>
</form>
