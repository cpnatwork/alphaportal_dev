package alpha.portal.service.impl;

import java.util.List;

import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.PayloadDao;
import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentRules;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.PayloadManager;

/**
 * implements the interface PayloadManager.
 * 
 * @see PayloadManager Payload Manager
 */
@Service("payloadManager")
public class PayloadManagerImpl extends GenericManagerImpl<Payload, PayloadIdentifier> implements PayloadManager {
    @Autowired
    private AlphaCardManager alphaCardManager;

    private PayloadDao payloadDao;

    public void setAlphaCardManager(final AlphaCardManager manager) {
        this.alphaCardManager = manager;
    }

    @Autowired
    public PayloadManagerImpl(final PayloadDao payloadDao) {
        super(payloadDao);
        this.payloadDao = payloadDao;
    }

    /**
     * @see alpha.portal.service.PayloadManager#saveNewPayload(alpha.portal.model.Payload, alpha.portal.model.AlphaCard)
     */
    public Payload saveNewPayload(Payload payload, final AlphaCard card) {
        if (!alphaCardManager.exists(card.getAlphaCardIdentifier()))
            throw new IllegalArgumentException("Card does not exist!");

        // take existing payloadId if set - else dao.save() will generate one
        if (card.getPayload() != null) {
            payload.getPayloadIdentifier().setPayloadId(card.getPayload().getPayloadIdentifier().getPayloadId());
        }

        // set payload now for AdornmentRules.getDataProvisionStatus
        card.setPayload(payload);

        // increment adornment "Payload Version"
        Adornment adPayloadVersion = card.getAlphaCardDescriptor().getAdornment(
                AdornmentType.PayloadVersionNumber.getName());
        if (adPayloadVersion == null) {
            adPayloadVersion = new Adornment(AdornmentType.PayloadVersionNumber.getName());
            adPayloadVersion.setValue("0");
        }
        int currentPayloadVersion = 0;
        try {
            currentPayloadVersion = Integer.parseInt(adPayloadVersion.getValue());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(adPayloadVersion.getValue());
        }
        adPayloadVersion.setValue(Integer.toString(++currentPayloadVersion));
        card.getAlphaCardDescriptor().setAdornment(adPayloadVersion);

        // update DataProvision
        card.getAlphaCardDescriptor().setAdornment(AdornmentType.DataProvision.getName(),
                AdornmentRules.getDataProvisionStatus(card));

        // save payload first so we have a valid identifier
        payload = super.save(payload);
        // then save its connection to the AlphaCard
        card.setPayload(payload);
        /* card = */alphaCardManager.save(card);
        return payload;
    }

    /**
     * @see alpha.portal.service.PayloadManager#getAllVersions()
     */
    public List<Payload> getAllVersions(final Payload payload) {
        return payloadDao.getAllVersions(payload);
    }

    /**
     * @see alpha.portal.service.PayloadManager#getVersion(int)
     */
    public Payload getVersion(final PayloadIdentifier id) {
        return payloadDao.getVersion(id);
    }

}
