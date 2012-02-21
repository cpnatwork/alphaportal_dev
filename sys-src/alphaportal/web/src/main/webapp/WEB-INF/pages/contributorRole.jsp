<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="contributorRoles.title"/></title>
    <meta name="heading" content="<fmt:message key='contributorRoles.heading'/>"/>
</head>

 <c:if test="${not empty status.errorMessages}">
 	<div class="error">    
 		<c:forEach var="error" items="${status.errorMessages}">
 			<img src="<c:url value="/images/iconWarning.gif"/>" alt="<fmt:message key="icon.warning"/>" class="icon" />
        	 <c:out value="${error}" escapeXml="false"/><br />
    	</c:forEach>
 	</div>
 </c:if>


<c:if test="${empty showEditingForm}">
	<display:table name="contributorRolesList" class="table" requestURI="" id="contributorRolesList">
		<display:column property="name" sortable="true" titleKey="contributorRoles.name"/>
		<display:column sortable="false" href="contributorRole" media="html" paramId="edit" paramProperty="contributorRoleId" titleKey="contributorRoles.edit_role_heading">
			<fmt:message key='contributorRoles.edit_role'/>
		</display:column>
		<display:column sortable="false" href="contributorRole" media="html" paramId="delete" paramProperty="contributorRoleId" titleKey="contributorRoles.delete_role_heading">
			<fmt:message key='contributorRoles.delete_role'/>
		</display:column>
		
		<display:setProperty name="paging.banner.item_name"><fmt:message key="contributorRoles.role"/></display:setProperty>
		<display:setProperty name="paging.banner.items_name"><fmt:message key="contributorRoles.roles"/></display:setProperty>
	</display:table>
		
	<br />
	
	<div class="leftBox">
		<h1><fmt:message key='contributorRoles.add_new'/></h1>
		
		<form method="post" action="contributorRole" id="contributorRolesForm" accept-charset="ISO-8859-1">
		<ul>
		    <li>
		        <appfuse:label styleClass="desc" key="contributorRoles.new_field"/>
		        <input type="text" name="newContributorRole" />
		    </li>
		
		    <li class="buttonBar bottom">
		       	<input type="submit" class="button" name="save_new" value="<fmt:message key="button.save"/>"/>
		    </li>
		</ul>
		</form>
	</div>

</c:if>


<c:if test="${not empty showEditingForm}">
	<div class="leftBox">
		<h1><fmt:message key='contributorRoles.edit'/></h1>
		
		<c:if test="${not empty messageId}">
			<c:if test="${messageId == 'edit_err_exists'}">
				<h2>
					<span style="color: #dd0000;"><fmt:message key='contributorRoles.edit_err_exists'/></span>
				</h2>
			</c:if>
			<c:if test="${messageId == 'edit_err_empty'}">
				<h2>
					<span style="color: #dd0000;"><fmt:message key='contributorRoles.edit_err_empty'/></span>
				</h2>
			</c:if>
		</c:if>
		
		<form method="post" action="contributorRole" id="contributorRolesForm" accept-charset="ISO-8859-1">
		<input type="hidden" name="oldContribRoleId" value="${roleToEditId}" />
		<ul>
		    <li>
		        <appfuse:label styleClass="desc" key="contributorRoles.edit_field"/>
		        <input type="text" name="newContributorRole" value="${roleToEdit}" />
		    </li>
		
		    <li class="buttonBar bottom">
		       	<input type="submit" class="button" name="save_edit" value="<fmt:message key="button.save"/>"/>
		       	<input type="submit" class="button" name="btn_cancel" value="<fmt:message key="button.cancel"/>"/>
		    </li>
		</ul>
		</form>
	</div>
</c:if>