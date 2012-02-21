package alpha.portal.webapp.util;

public enum CardFilterContributorRole {
    ALL("--Fachgebiet des Verantwortlichen"), OWN("eigene"), OTHERS("andere");

    private String name;

    private CardFilterContributorRole(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
