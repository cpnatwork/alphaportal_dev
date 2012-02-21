<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="addUser.title"/></title>
    <meta name="heading" content="<fmt:message key='addUser.heading'/>"/>
</head>

<c:set var="checkAll">
    <input type="checkbox" name="allbox" onclick="checkAll(this.form)" style="margin: 0 0 0 4px" />
</c:set>
<%
 String lastName ="";  
 if(request.getParameter("lastName")!=null) {  
  lastName = request.getParameter("lastName");
 }
%> 
<form method="post" id="userSearch">
	<ul>
		<li>
	        <appfuse:label styleClass="desc" key="adduser.name"/>
	        <input name="lastName" class="text medium" value="<%= lastName %>" maxlength="50"/>
	    </li>
	    
	    <li class="buttonBar bottom">
	       	<input type="submit" class="button" name="search" value="<fmt:message key="button.search"/>"/>
	        <input type="submit" class="button" name="cancel" value="<fmt:message key="button.cancel"/>" onclick="bCancel=true"/>
    	</li>
    	
    	<c:if test="${pageContext.request.method == 'POST'}">
	    <li>
	    	<display:table name="users" class="table" requestURI="" id="row">
		    	<display:column>
				    <input type="checkbox" name="sel[]" value="<c:out value="${row.id}"/>" />
				</display:column>
		    	
	    		<display:column property="lastName" media="html" titleKey="user.lastName"/>
	    		<display:column property="firstName" media="html" titleKey="user.firstName"/>
	    		<display:setProperty name="paging.banner.item_name"><fmt:message key="cardList.card"/></display:setProperty>
	    		<display:setProperty name="paging.banner.items_name"><fmt:message key="cardList.cards"/></display:setProperty>
			</display:table>
			<c:if test="${not empty users}">
				<input type="submit" name="add" value="<fmt:message key="button.add"/>"/>
			</c:if>
		</li>
	</c:if>
	</ul>
</form>
