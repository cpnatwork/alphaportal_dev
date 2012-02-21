/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
package alpha.portal.service;

import java.util.List;

import javax.jws.WebService;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import alpha.portal.model.AlphaCase;

/**
 * The Interface CaseService.
 */
@WebService
@Path("/caseservice")
@Produces({ "application/json" })
public interface CaseService {

	/**
	 * Retrieves a case by caseId. An exception is thrown if user not found
	 * 
	 * @param caseId
	 *            the identifier for the case
	 * @return the AlphaCase
	 */
	@Path("/case/{id}")
	@GET
	public AlphaCase getCase(@PathParam("id") String caseId);

	/**
	 * Retrieves a list of all cases.
	 * 
	 * @return List of Cases
	 */
	@GET
	@Path("/cases")
	List<AlphaCase> getCases();

	/**
	 * Saves a case.
	 * 
	 * @param alphaCase
	 *            the alpha case
	 * @return updated case
	 */
	@POST
	@Path("/case")
	AlphaCase saveCase(AlphaCase alphaCase);

	// TODO
	// /**
	// * Update the card-order of a case by a list of Ids
	// */
	// @POST
	// @Path("/case/{id}/setCardOrder")
	// void setCardOrder(String[] cardOrder);

	/**
	 * Removes a case from the database by caseId.
	 * 
	 * @param caseId
	 *            the caseId
	 */
	@DELETE
	@Path("/case/{id}")
	void removeCase(@PathParam("id") String caseId);

}
