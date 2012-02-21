package alpha.portal.service;

import javax.jws.WebService;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import alpha.portal.model.Adornment;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.Payload;

@WebService
@Path("/cardservice")
@Produces( { "application/json" })
public interface AlphaCardService {

    /**
     * Retrieves a card by cardId.
     * 
     * @param cardId
     *            the identifier for the case
     * @return the AlphaCard
     */
    @Path("/case/{caseId}/card/{cardId}/")
    @GET
    public AlphaCard getCard(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId);

    /**
     * Adds / Creates a card
     * 
     * @return added card
     */
    @POST
    @Path("/case/{caseId}/card")
    AlphaCard addCard(AlphaCard alphaCard);

    /**
     * Deletes a alphaCard from the database by cardId and caseId
     * 
     * @param cardId
     *            the cardId
     * @param caseId
     *            the caseId
     */
    @DELETE
    @Path("/case/{caseId}/card/{cardId}")
    void deleteCard(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId);

    /**
     * Get an Adornment by Key and cardId / casesId.
     * 
     * @param cardId
     *            the cardId
     * @param caseId
     *            the caseId
     * @param adornmentName
     *            the adornments name
     * @return the specify adornment or null
     */
    @GET
    @Path("/case/{caseId}/card/{cardId}/adornment/{adornmentName}")
    Adornment getAdornment(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId,
            @PathParam("adornmentName") String adornmentName);

    /**
     * Add an Adornment to a alphaCard.
     * 
     * @param cardId
     *            the alphaCards-Id
     * @param caseId
     *            the alphaCase of the alphaCard
     * @param adornment
     *            the adornment to add
     * @return the added adornment
     */
    @POST
    @Path("/case/{caseId}/card/{cardId}/adornment")
    Adornment addAdornment(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId, Adornment adornment);

    /**
     * Delete an adornment by name.
     * 
     * @param cardId
     *            the cardId
     * @param caseId
     *            the caseId
     * @param adornmentName
     *            the adornments name
     */
    @DELETE
    @Path("/case/{caseId}/card/{cardId}/adornment/{adornmentName}")
    void deleteAdornment(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId,
            @PathParam("adornmentName") String adornmentName);

    /**
     * Get the current payload.
     * 
     * @param cardId
     *            the cardId
     * @param caseId
     *            the caseId
     * @return the Payload object
     */
    @GET
    @Path("/case/{caseId}/card/{cardId}/payload")
    Payload getPayload(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId);

    /**
     * Sets the Payload
     * 
     * @param cardId
     *            the cardId
     * @param caseId
     *            the caseId
     * @param payload
     *            the payload to set
     * @return the saved payload object
     */
    @POST
    @Path("/case/{caseId}/card/{cardId}/payload")
    Payload setPayload(@PathParam("cardId") String cardId, @PathParam("caseId") String caseId, Payload payload);

}
