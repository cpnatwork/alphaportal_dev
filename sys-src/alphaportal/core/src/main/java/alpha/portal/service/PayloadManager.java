package alpha.portal.service;

import java.util.List;

import org.appfuse.service.GenericManager;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;

/**
 * Interface PayloadManager, which extends from the GenericManager with following objects: Payload and Long.
 */
public interface PayloadManager extends GenericManager<Payload, PayloadIdentifier> {

    /**
     * Gets all payload versions from the database
     * 
     * @param payload
     *            Payload
     * @return List with Payloads
     */
    public List<Payload> getAllVersions(Payload payload);

    /**
     * Gets the given version of the payload
     * 
     * 
     * @param id
     *            PayloadIdentifier
     * @return the Payload with the given version
     */
    public Payload getVersion(PayloadIdentifier id);

    /**
     * Saves a new payload in to the given AlphaCard. Generates a payloadId if there is none.
     * 
     * @return the saved payload (with its new sequenceNumber)
     */
    public Payload saveNewPayload(final Payload payload, final AlphaCard card);
}
