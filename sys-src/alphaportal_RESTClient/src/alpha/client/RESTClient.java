package alpha.client;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.UnexpectedException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

import javax.ws.rs.core.Response;
import javax.ws.rs.ext.MessageBodyReader;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.cxf.bus.extension.ExtensionManagerImpl;
import org.apache.cxf.common.logging.LogUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.codehaus.jackson.jaxrs.JacksonJaxbJsonProvider;

import alpha.portal.model.Adornment;
import alpha.portal.model.Payload;

public class RESTClient {

    private static final String OPTION_DEBUG = "debug";
    private static final String OPTION_SET = "set";
    private static final String OPTION_GET = "get";
    private static final String OPTION_DELETE = "delete";
    private static final String OPTION_ADD = "add";
    private static final String OPTION_VERSION = "v";
    private static final String OPTION_PAYLOADPATH = "path";
    private static final String OPTION_PAYLOADNAME = "pName";
    private static final String OPTION_ADORNMENTVALUE = "aValue";
    private static final String OPTION_ADORNMENTNAME = "aName";
    private static final String OPTION_CONTRIBUTOR = "coid";
    private static final String OPTION_CARD = "cdid";
    private static final String OPTION_HELP = "h";
    private static final String OPTION_HOST = "hn";
    private static final String OPTION_CASE = "csid";

    private static final String PAYLOADTYPE = "payload";
    private static final String ADORNMENTTYPE = "adornment";
    private static final int BUFFER_SIZE = 1024;

    /**
     * Returns the Options of the CommandLine
     * 
     * @return returns the options of CommonCli
     */
    @SuppressWarnings("static-access")
    private static Options getOptions() {
        OptionGroup group = new OptionGroup();
        Options options = new Options();

        Option help = new Option(OPTION_HELP, "help", false, "shows all possible options");

        Option debug = new Option(OPTION_DEBUG, false, "sets the Level of the Logging off");

        Option setHostName = OptionBuilder.withLongOpt("hostName").withArgName("HostID").hasArg().withDescription(
                "sets the hostName ").create(OPTION_HOST);

        Option setCaseId = OptionBuilder.withLongOpt("caseID").withArgName("AlphaCaseID").hasArg().withDescription(
                "sets the caseID ").create(OPTION_CASE);

        Option setCardId = OptionBuilder.withLongOpt("cardID").withArgName("AlphaCardID").hasArg().withDescription(
                "sets the cardID ").create(OPTION_CARD);

        Option setContributorId = OptionBuilder.withLongOpt("contributerID").withArgName("ContributorID").hasArg()
                .withDescription("sets the contributorID ").create(OPTION_CONTRIBUTOR);

        Option setAdornmentName = OptionBuilder.withLongOpt("adornmentName").withArgName("AdornmentName").hasArg()
                .withDescription("sets the adornmentName ").create(OPTION_ADORNMENTNAME);

        Option setAdornmentValue = OptionBuilder.withLongOpt("adornmentValue").withArgName("AdornmentValue").hasArg()
                .withDescription("sets the adornmentValue ").create(OPTION_ADORNMENTVALUE);

        Option setPayloadName = OptionBuilder.withLongOpt("payloadName").withArgName("PayloadName").hasArg()
                .withDescription("sets the payloadName ").create(OPTION_PAYLOADNAME);

        Option setPayloadPath = OptionBuilder.withLongOpt("payloadPath").withArgName("PayloadPath").hasArg()
                .withDescription("sets the payloadPath ").create(OPTION_PAYLOADPATH);

        Option setVersion = OptionBuilder.withLongOpt("version").withArgName("VersionID").hasArg().withDescription(
                "sets the versionID ").create(OPTION_VERSION);

        options.addOption(setCaseId);
        options.addOption(setCardId);
        options.addOption(setContributorId);
        options.addOption(setAdornmentName);
        options.addOption(setAdornmentValue);
        options.addOption(setPayloadName);
        options.addOption(setPayloadPath);
        options.addOption(setVersion);
        options.addOption(setHostName);
        options.addOption(help);

        group.addOption(OptionBuilder.withDescription("adds an adornment or a payload").create(OPTION_ADD));
        group.addOption(OptionBuilder.withDescription("deletes an adornment or a payload").create(OPTION_DELETE));
        group.addOption(OptionBuilder.withDescription("gets an adornment or a payload").create(OPTION_GET));
        group.addOption(OptionBuilder.withDescription("sets an adornment or a payload").create(OPTION_SET));

        options.addOptionGroup(group);
        return options;
    }

    /**
     * reads the file and transforms it in a payload
     * 
     * @param path
     *            path of the file
     * @param name
     *            name as which the payload should be saved
     * @return a payload
     */
    private static Payload readFile(String path, Payload payload) {
        InputStream inputStream = null;

        try {
            inputStream = new FileInputStream(path);
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // Attachment att = new Attachment("root", inputStream, new ContentDisposition("attachment;"));
        ByteArrayOutputStream byteBuffer = new ByteArrayOutputStream();
        byte[] buffer = new byte[BUFFER_SIZE];

        int readBytes = 0;
        try {
            while (true) {
                readBytes = inputStream.read(buffer);
                if (readBytes > 0) {
                    byteBuffer.write(buffer, 0, readBytes);
                } else {
                    break;
                }
            }
        } catch (IOException e) {
            // TODO: handle exception
            e.printStackTrace();
        }

        payload.setContent(byteBuffer.toByteArray());

        return payload;
    }

    /**
     * builds the Request and returns a Response
     * 
     * @param baseAddress
     *            baseAddress
     * @param type
     *            HTTP Content-Type header
     * @param accept
     *            HTTP Accept Header
     * @param methodType
     *            method, either "add", "delete", "get" or "set
     * @param nature
     *            defines if it is an adornment or a payload
     * @param name
     *            name of an adornment or a payload
     * @param value
     *            value of the adornment or the path of the payload
     * @return Response
     * @throws UnexpectedException
     */
    @SuppressWarnings("unused")
    private static Object buildRequest(String host, String baseAddress, String type, String accept, String methodType,
            String nature, String name, String value, String version) throws UnexpectedException {

        Adornment adornment = null;
        Payload payload = null;

        baseAddress = baseAddress + nature;

        if (nature.equals(ADORNMENTTYPE)) {
            adornment = new Adornment(name);

            if (value != null) {
                adornment.setValue(value); // sets the value, if one exists
            }

            if (!methodType.equals("POST")) {
                baseAddress = baseAddress + "/" + name;
            }
        } else if (nature.equals(PAYLOADTYPE)) {
            payload = new Payload(name, "application/octet-stream");

            if (value != null) {
                payload = readFile(value, payload); // reads the file, if one is attached
            }
        }
        MessageBodyReader<?> p = new JacksonJaxbJsonProvider();
        List<MessageBodyReader<?>> prov = new LinkedList<MessageBodyReader<?>>();
        prov.add(p);

        WebClient client = WebClient.create("http://" + host, prov);
        client.path(baseAddress);
        client.type(type).accept(accept);
        Object r = null;
        InputStream input;

        if (methodType.equals("POST")) { // adds the given Object
            if (nature.equals(ADORNMENTTYPE)) {
                r = client.post(adornment);
                System.out.println("Adornment successfully posted.");
            } else if (nature.equals(PAYLOADTYPE)) {
                r = client.post(payload);
                System.out.println("Payload successfully posted.");
            }
        } else if (methodType.equals("DELETE")) { // to delete the needed Object, the ids must be set in the baseAdress
            r = client.delete();

            if (((Response) r).getStatus() < 200 || ((Response) r).getStatus() > 299) {
                throw new UnexpectedException("Cannot delete");
            } else {
                System.out.println("Adornment successfully deleted.");
            }
        } else if (methodType.equals("GET")) { // to get the needed Object, the ids must be set in the baseAdress
            if (nature.equals(ADORNMENTTYPE)) {
                r = client.get(Adornment.class);
            } else if (nature.equals(PAYLOADTYPE)) {
                r = client.get(Payload.class);
            }
        }

        return r;
    }

    public static void main(String[] args) throws IOException {
        CommandLineParser parser = new PosixParser();
        HelpFormatter formater = new HelpFormatter();
        CommandLine line = null;
        Options options = getOptions();

        try {
            line = parser.parse(options, args);

            String host = line.getOptionValue(OPTION_HOST);
            String caseId = line.getOptionValue(OPTION_CASE);
            String cardId = line.getOptionValue(OPTION_CARD);
            String contributorId = line.getOptionValue(OPTION_CONTRIBUTOR);
            String adornmentName = line.getOptionValue(OPTION_ADORNMENTNAME);
            String adornmentValue = line.getOptionValue(OPTION_ADORNMENTVALUE);
            String payloadName = line.getOptionValue(OPTION_PAYLOADNAME);
            String payloadPath = line.getOptionValue(OPTION_PAYLOADPATH);
            String version = line.getOptionValue(OPTION_VERSION);

            String baseAddress = "/services/api/cardservice/case/" + caseId + "/card/" + cardId + "/";
            String type = "application/json";
            String accept = "application/json";
            Object result = null;

            if (!options.hasOption(OPTION_DEBUG)) {
                LogUtils.getL7dLogger(ExtensionManagerImpl.class).setLevel(Level.OFF);
            }

            // handling of the single groups
            if (line.hasOption(OPTION_HELP)) {
                formater.printHelp(RESTClient.class.toString(), options);
                return;
            } else if (line.hasOption(OPTION_ADD) || line.hasOption(OPTION_SET)) {
                if (host != null && caseId != null && cardId != null) {
                    if (contributorId != null) {
                        result = buildRequest(host, baseAddress, type, accept, "POST", ADORNMENTTYPE, "Contributor",
                                contributorId, version);
                    } else if (adornmentName != null && adornmentValue != null) {
                        result = buildRequest(host, baseAddress, type, accept, "POST", ADORNMENTTYPE, adornmentName,
                                adornmentValue, version);
                    } else if (payloadName != null && payloadPath != null) {
                        result = buildRequest(host, baseAddress, type, accept, "POST", PAYLOADTYPE, payloadName,
                                payloadPath, version);
                    } else {
                        System.out.println("ADD/SET: You forgot to set the adornments or the payload attributes");
                    }
                } else {
                    System.out.println("ADD/SET: You forgot to enter the Hostname or the CardId or the CaseId.");
                }
            } else if (line.hasOption(OPTION_DELETE)) {
                if (host != null && caseId != null && cardId != null) {
                    if (contributorId != null) {
                        result = buildRequest(host, baseAddress, type, accept, "DELETE", ADORNMENTTYPE, "Contributor",
                                contributorId, version);
                    } else if (adornmentName != null) {
                        result = buildRequest(host, baseAddress, type, accept, "DELETE", ADORNMENTTYPE, adornmentName,
                                adornmentValue, version);
                    } else {
                        System.out.println("DELETE: You forgot to set the adornments attributes.");
                    }
                } else {
                    System.out.println("DELETE: You forgot to enter the Hostname or the CardId or the CaseId.");
                }
            } else if (line.hasOption(OPTION_GET)) {
                if (host != null && caseId != null && cardId != null) {
                    if (contributorId != null) {
                        result = buildRequest(host, baseAddress, type, accept, "GET", ADORNMENTTYPE, "Contributor",
                                contributorId, version);
                    } else if (adornmentName != null) {
                        result = buildRequest(host, baseAddress, type, accept, "GET", ADORNMENTTYPE, adornmentName,
                                adornmentValue, version);
                    } else if (payloadName != null) {
                        result = buildRequest(host, baseAddress, type, accept, "GET", PAYLOADTYPE, payloadName,
                                payloadPath, version);
                    } else {
                        System.out.println("GET: You forgot to set the adornments or the payload attributes.");
                    }
                } else {
                    System.out.println("GET: You forgot to enter the Hostname or the CardId or the CaseId.");
                }
            } else {
                System.out.println("Please enter the action you want to do: add, get, set or delete.");
            }

            if (result instanceof Adornment) {
                System.out.println(result.toString());
            } else if (result instanceof Payload) {
                Payload p = (Payload) result;
                FileOutputStream f = new FileOutputStream(p.getFilename());
                f.write(p.getContent());
                f.close();
                System.out.println("Wrote to file " + p.getFilename());
            }
        } catch (ParseException exp) {
            System.out.println("Unexpected exception:" + exp.getMessage());
        }
    }
}
