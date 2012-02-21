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

@WebService
@Path("/caseservice")
@Produces( { "application/json" })
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
     * Saves a case
     * 
     * @param case the case to save
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
     * Removes a case from the database by caseId
     * 
     * @param caseId
     *            the caseId
     */
    @DELETE
    @Path("/case/{id}")
    void removeCase(@PathParam("id") String caseId);

}
