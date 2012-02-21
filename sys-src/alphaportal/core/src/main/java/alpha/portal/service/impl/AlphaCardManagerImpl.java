package alpha.portal.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.appfuse.service.impl.GenericManagerImpl;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDataProvision;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardDescriptor;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.AlphaCardService;
import alpha.portal.service.CaseManager;
import alpha.portal.service.PayloadManager;

/**
 * implements the interface AlphaCardManager.
 * 
 * @see AlphaCardManager α-Card Manager
 */
@Service("alphaCardManager")
public class AlphaCardManagerImpl extends GenericManagerImpl<AlphaCard, AlphaCardIdentifier> implements
        AlphaCardManager, AlphaCardService {

    public static final Criterion _PROVISION = Restrictions.eq("ad.name", AdornmentType.DataProvision.getName());
    public static final Criterion _PROVISION_OPEN = Restrictions
            .eq("ad.value", AdornmentTypeDataProvision.OPEN.value());
    public static final Criterion _PROVISION_INPROGRESS = Restrictions.eq("ad.value",
            AdornmentTypeDataProvision.INPROGRESS.value());
    public static final Criterion _PROVISION_OPEN_INPROGRESS = Restrictions.or(_PROVISION_OPEN, _PROVISION_INPROGRESS);
    public static final Criterion _PROVISION_FULFILLED = Restrictions.eq("ad.value",
            AdornmentTypeDataProvision.FULLFILLED.value());
    public static final Criterion _CONTRIBUTOR = Restrictions.eq("ad.name", AdornmentType.Contributor.getName());
    public static final Criterion _CONTRIBUTORROLE = Restrictions
            .eq("ad.name", AdornmentType.ContributorRole.getName());

    private static final Criterion _DELETE_STATUS = Restrictions.eq("ad.name", AdornmentType.Deleted.getName());
    private static final Criterion _NOT_DELETED = Restrictions.eq("ad.value", AdornmentTypeDeleted.FALSE.value());
    private static final Criterion _DELETED = Restrictions.eq("ad.value", AdornmentTypeDeleted.TRUE.value());

    /**
     * Restriction for the filter element "open"
     */
    public static final Criterion DATA_PROVISION_OPEN = Restrictions.and(_PROVISION, _PROVISION_OPEN);

    /**
     * Restriction for the filter element "open or inprogress"
     */
    public static final Criterion DATA_PROVISION_OPEN_INPROGRESS = Restrictions.and(_PROVISION,
            _PROVISION_OPEN_INPROGRESS);

    /**
     * Restriction for the filter element "inprogress"
     */
    public static final Criterion DATA_PROVISION_INPROGRESS = Restrictions.and(_PROVISION, _PROVISION_INPROGRESS);

    /**
     * Restriction for the filter element "fulfilled"
     */
    public static final Criterion DATA_PROVISION_FULFILLED = Restrictions.and(_PROVISION, _PROVISION_FULFILLED);

    /**
     * Deleted-Restriction: Restriction for the filter element "notDeleted"
     */
    public static final Criterion NOT_DELETED = Restrictions.and(_DELETE_STATUS, _NOT_DELETED);

    /**
     * Deleted-Restriction: Restriction for the filter element "deleted"
     */
    public static final Criterion DELETED = Restrictions.and(_DELETE_STATUS, _DELETED);

    @Autowired
    PayloadManager payloadManager;

    @Autowired
    CaseManager caseManager;

    @Autowired
    public AlphaCardManagerImpl(final AlphaCardDao alphaCardDao) {
        super(alphaCardDao);
    }

    public void setPayloadManager(final PayloadManager payloadManager) {
        this.payloadManager = payloadManager;
    }

    /**
     * @see alpha.portal.service.AlphaCardManager#createAlphaCard(java.lang.String)
     */
    public AlphaCard createAlphaCard(final String caseId) {
        AlphaCard alphaCard = new AlphaCard(caseId);
        AlphaCardDescriptor descriptor = alphaCard.getAlphaCardDescriptor();

        for (AdornmentType type : AdornmentType.values()) {
            descriptor.setAdornment(type.getName(), type.getValueDefault());
        }
        return alphaCard;
    }

    /**
     * @see alpha.portal.service.AlphaCardManager#getVersion(alpha.portal.model.AlphaCardIdentifier)
     */
    public AlphaCard getVersion(final AlphaCardIdentifier id) {
        return ((AlphaCardDao) dao).getVersion(id);
    }

    /**
     * @see alpha.portal.service.AlphaCardManager#getAllVersions(java.lang.String, java.lang.String)
     */
    public List<AlphaCard> getAllVersions(final String caseId) {
        return ((AlphaCardDao) dao).getAllVersions(caseId);
    }

    /**
     * Overridden to save the AlphaCase, too, to connect it with the newest version.
     */
    @Override
    public AlphaCard save(AlphaCard card) {
        AlphaCase aCase = card.getAlphaCase();
        int pos = aCase.getAlphaCards().indexOf(card);
        if (pos > -1) {
            card.getAlphaCase().removeAlphaCard(card);
        }
        card = ((AlphaCardDao) dao).save(card);
        if (pos > -1) {
            aCase.addAlphaCard(card, pos);
        } else {
            aCase.addAlphaCard(card);
        }
        card.setAlphaCase(caseManager.save(aCase));
        return card;
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#getCard(String, String)
     */
    public AlphaCard getCard(final String cardId, final String caseId) {
        return get(new AlphaCardIdentifier(caseId, cardId));
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#addCard(AlphaCard)
     */
    public AlphaCard addCard(final AlphaCard alphaCard) {
        return saveCard(alphaCard);
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#saveCard(AlphaCard)
     */
    public AlphaCard saveCard(final AlphaCard alphaCard) {
        return save(alphaCard);
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#deleteCard(String, String)
     */
    public void deleteCard(final String cardId, final String caseId) {
        remove(new AlphaCardIdentifier(caseId, cardId));
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#getAdornment(String, String, String)
     */
    public Adornment getAdornment(final String cardId, final String caseId, final String adornmentName) {
        AlphaCard alphaCard = getCard(cardId, caseId);
        if (alphaCard != null) {
            AlphaCardDescriptor descriptor = alphaCard.getAlphaCardDescriptor();
            return descriptor.getAdornment(adornmentName);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#addAdornment(String, String, Adornment)
     */
    public Adornment addAdornment(final String cardId, final String caseId, final Adornment adornment) {
        return saveAdornment(cardId, caseId, adornment);
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#saveAdornment(String, String, Adornment)
     */
    public Adornment saveAdornment(final String cardId, final String caseId, Adornment adornment) {
        AlphaCard alphaCard = getCard(cardId, caseId);
        if (alphaCard != null) {
            AlphaCardDescriptor descriptor = alphaCard.getAlphaCardDescriptor();
            descriptor.setAdornment(adornment);
            alphaCard = save(alphaCard);
            adornment = alphaCard.getAlphaCardDescriptor().getAdornment(adornment.getName());
        }
        return adornment;
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#deleteAdornment(String, String, String)
     */
    public void deleteAdornment(final String cardId, final String caseId, final String adornmentName) {
        AlphaCard alphaCard = getCard(cardId, caseId);
        if (alphaCard != null) {
            AlphaCardDescriptor descriptor = alphaCard.getAlphaCardDescriptor();
            if (descriptor.deleteAdornment(adornmentName)) {
                save(alphaCard);
            }
        }
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#getPayload(String, String)
     */
    public Payload getPayload(final String cardId, final String caseId) {
        AlphaCard alphaCard = getCard(cardId, caseId);
        if (alphaCard != null)
            return alphaCard.getPayload();
        return null;
    }

    /**
     * {@inheritDoc}
     * 
     * @see AlphaCardService#setPayload(String, String, Payload)
     */
    public Payload setPayload(final String cardId, final String caseId, final Payload payload) {
        AlphaCard alphaCard = getCard(cardId, caseId);
        if (alphaCard != null && payload != null)
            return payloadManager.saveNewPayload(payload, alphaCard);
        return null;
    }

    /**
     * {@inheritDoc}
     * 
     * @see alpha.portal.service.AlphaCardManager#listAlphaCardsByCriterion(org.hibernate.criterion.Criterion[])
     */
    public List<AlphaCard> listAlphaCardsByCriterion(final String caseId, final Criterion... criterions) {
        if (StringUtils.isBlank(caseId))
            return null;

        return ((AlphaCardDao) dao).listAlphaCardsByCriterion(caseId, criterions);
    }

    /**
     * Returns a {@link Criterion} to get all Adornments with the given userId as contributor.
     * 
     * @param userId
     *            the users id
     * @return a {@link Criterion}
     */
    public static Criterion getContributorCriterionOwn(final String userId) {
        return Restrictions.and(_CONTRIBUTOR, Restrictions.eq("ad.value", userId));
    }

    /**
     * Returns a {@link Criterion} to get all Adornments where the given userId is not the contributor.
     * 
     * @param userId
     *            the users id
     * @return a {@link Criterion}
     */
    public static Criterion getContributorCriterionOthers(final String userId) {
        return Restrictions.and(_CONTRIBUTOR, Restrictions.ne("ad.value", userId));
    }

    /**
     * Returns a {@link Criterion} to get all Adornments where the contributorrole is one of roles
     * 
     * @param userId
     *            the users id
     * @return a {@link Criterion}
     */
    public static Criterion getContributorRoleCriterionOwn(final String... roles) {
        return Restrictions.and(_CONTRIBUTORROLE, Restrictions.in("ad.value", roles));
    }

    /**
     * Returns a {@link Criterion} to get all Adornments where the contributorrole is not in roles
     * 
     * @param userId
     *            the users id
     * @return a {@link Criterion}
     */
    public static Criterion getContributorRoleCriterionOthers(final String... roles) {
        return Restrictions.and(_CONTRIBUTORROLE, Restrictions.not(Restrictions.in("ad.value", roles)));
    }

    /**
     * Get the last 20 changes of AlphaCards for the Dashboard.
     * 
     * @param caseList
     *            A list with the cases where I take part.
     * 
     * @return A list with AlphaCards
     */
    public List<AlphaCard> listDashBoardCards(final List<AlphaCase> caseList) {
        List<AlphaCard> result = new ArrayList<AlphaCard>();

        String[] caseIDs = new String[caseList.size()];
        int i = 0;
        for (AlphaCase ac : caseList) {
            caseIDs[i] = ac.getCaseId();
            i++;
        }

        return ((AlphaCardDao) dao).listDashBoardAlphaCards(caseIDs);
    }

    public CaseManager getCaseManager() {
        return caseManager;
    }

    public void setCaseManager(final CaseManager caseManager) {
        this.caseManager = caseManager;
    }
}
