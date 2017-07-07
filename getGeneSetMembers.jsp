<%@page contentType="text/html; charset=UTF-8"%>
<%@page import="org.json.simple.JSONArray"%>
<%@page import="java.sql.*"%>
<%@page import="java.util.Vector"%>
<%@page import="java.util.Iterator"%>
<%@page import="java.io.FileNotFoundException"%>
<%@page import="java.io.IOException"%>
<%@page import="java.io.InputStream"%>
<%@page import="java.util.Properties"%>

<%
	String geneSetName = request.getParameter("geneSetName");
	
	//String geneSetName = "KEGG_GLYCOLYSIS_GLUCONEOGENESIS";
	
	String geneSetCollectionValue = request.getParameter("geneSetCollection");
	//String geneSetCollectionValue = "MSigDBC2CP";
	
	geneSetName= geneSetName.trim();
	geneSetName="\""+geneSetName+"\"";
	
	String tblName = null;
	if (geneSetCollectionValue.equals("pathprint")) {
		tblName = "pathprint_geneset_tbl";
	} else if (geneSetCollectionValue.equals("MSigDBC2CP")) {
		tblName = "msigdb_c2_cp_geneset_tbl";
	} else if (geneSetCollectionValue.equals("MSigDBC5")) {
		tblName = "msigdb_c5_go_geneset_tbl";
	} else if (geneSetCollectionValue.equals("MSigDBH")) {
		tblName = "msigdb_h_hallmark_geneset_tbl";
	} else {
		tblName = null;
	}
	
	String JDBC_DRIVER = "com.mysql.jdbc.Driver";  
	String DB_URL = "jdbc:mysql://localhost/pcxn_database";

	//  Database credentials
	String USER = "";
	String PASS = "";
	
	try {
		Properties prop = new Properties();
		String propFileName = "pcxnconfig.properties"; 
		InputStream inputStream = getClass().getClassLoader().getResourceAsStream(propFileName); 
		if (inputStream != null) {
			prop.load(inputStream);
		} else {
			throw new FileNotFoundException("property file '" + propFileName + "' not found in the classpath");
		}
		USER = prop.getProperty("user");
		PASS = prop.getProperty("password");
	} catch (Exception e) {
		out.println("Exception: " + e);
	} finally {
	}

	Connection conn = null;
	Statement stmt = null;
	
	//String sql2 = "select * from human_gene_tbl where geneid in (select entrezid from " + tblName + "  where geneset_name = " + geneSetName +")";
	String sql2 = "select human_gene_tbl.* from human_gene_tbl inner join " + tblName + " on geneid=entrezid where geneset_name =" +  geneSetName;
	
	//String sql2 = "select * from human_gene_tbl where geneid in (select entrezid from pathprint_geneset_tbl where geneset_name = \"Caffeine metabolism (KEGG)\")";
	JSONArray matrix = new JSONArray();	
	try{
		//STEP 2: Register JDBC driver
		Class.forName("com.mysql.jdbc.Driver");

		//STEP 3: Open a connection
		conn = DriverManager.getConnection(DB_URL,USER,PASS);

		//STEP 4: Execute a query
		stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql2);

		//STEP 5: Extract data from result set
		while(rs.next()){
			//Retrieve by column name
			int geneid = rs.getInt("geneid");
			String symbol = rs.getString("symbol");
			String description = rs.getString("description");

			//store values
			JSONArray list = new JSONArray();
			list.add(geneid);
			list.add(symbol);
			list.add(description);
			matrix.add(list);
		}
		//STEP 6: Clean-up environment
		rs.close();
		stmt.close();
		conn.close();
	}catch(SQLException se){
		//Handle errors for JDBC
		se.printStackTrace();
	}catch(Exception e){
		//Handle errors for Class.forName
		e.printStackTrace();
	}finally{
		//finally block used to close resources
		try{
			if(stmt!=null)
				stmt.close();
		}catch(SQLException se2){
		}// nothing we can do
		try{
         if(conn!=null)
            conn.close();
		}catch(SQLException se){
			se.printStackTrace();
		}//end finally try
	}//end try
	out.print(matrix);
	out.flush();
%>