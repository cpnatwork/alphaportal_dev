package alpha.portal.dao;

import java.util.List;

import org.appfuse.dao.GenericDao;

import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;

/**
 * The PayloadDAO is the Data Access Object of the Payload Class. It extends from the GenericDao with the following
 * objects: Payload and Long.
 */
public interface PayloadDao extends GenericDao<Payload, PayloadIdentifier> {

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
     * @param id
     *            PayloadIdentifier
     * 
     * @return he Payload with the given version
     */
    public Payload getVersion(PayloadIdentifier id);
}
