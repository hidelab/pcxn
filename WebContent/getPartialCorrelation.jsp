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
	String genesetsText = request.getParameter("genesets");
	String geneSetCollectionValue = request.getParameter("geneSetCollection");
	double pCutOffValue = Double.parseDouble(request.getParameter("pCutOff"));
	float corrCutOffValue = Float.parseFloat(request.getParameter("corrCutOff"));
	int topNValue = Integer.parseInt(request.getParameter("topN"));
	genesetsText= genesetsText.trim();
	genesetsText=genesetsText.replace("\n", "\"\n\"");
	genesetsText="\""+genesetsText+"\"";
	genesetsText=genesetsText.replace("\n", ",");
	
	String tblName = null;
	if (geneSetCollectionValue.equals("pathprint")) {
		tblName = "pathprint_correlation_tbl";
	} else if (geneSetCollectionValue.equals("MSigDBC2CP")) {
		tblName = "msigdb_c2_cp_correlation_tbl";
	} else if (geneSetCollectionValue.equals("MSigDBC5")) {
		tblName = "msigdb_c5_go_correlation_tbl";
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
	
	if (topNValue > 0){
		try{
		//STEP 2: Register JDBC driver
		Class.forName("com.mysql.jdbc.Driver");

		//STEP 3: Open a connection
		conn = DriverManager.getConnection(DB_URL,USER,PASS);

		//STEP 4: Execute a query
		stmt = conn.createStatement();
		String sql1 = "select gene_set_B as gene_set_C, max(abs(partial_correlation)) as abs_partial_correlation from " + tblName + " where gene_set_A in ( " + genesetsText + ") AND gene_set_B not in (" + genesetsText + ") AND p_value < " + pCutOffValue + " AND abs(partial_correlation) > " + corrCutOffValue + " group by gene_set_C union select gene_set_A as gene_set_C, max(abs(partial_correlation)) as abs_partial_correlation from " + tblName + " where gene_set_A not in ( " + genesetsText + ") AND gene_set_B in (" + genesetsText + ") AND p_value < " + pCutOffValue + " AND abs(partial_correlation) > " + corrCutOffValue + " group by gene_set_C order by abs_partial_correlation desc";
		
		ResultSet rs = stmt.executeQuery(sql1);

		//STEP 5: Extract data from result set
		Vector gene_set_C_list = new Vector();
		while(rs.next() & gene_set_C_list.size()< topNValue){
			String gene_set_C = rs.getString("gene_set_C");			
			if (gene_set_C_list.indexOf(gene_set_C) ==-1) {
				gene_set_C_list.add(gene_set_C);
			}
		}
		
		Iterator theData = gene_set_C_list.iterator();
		while(theData.hasNext()) {
			genesetsText = genesetsText + ",\"" + theData.next() + "\"";
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
	}
	
	
	String sql2 = "select gene_set_A, gene_set_B, partial_correlation, p_value, overlap_coefficient from " + tblName + " where gene_set_A in ( " + genesetsText + ") AND gene_set_B in (" + genesetsText +")";
		
	JSONArray matrix = new JSONArray();	
	try{
		//STEP 2: Register JDBC driver
		Class.forName("com.mysql.jdbc.Driver");

		//STEP 3: Open a connection
		conn = DriverManager.getConnection(DB_URL,USER,PASS);

		//STEP 4: Execute a query
		stmt = conn.createStatement();
		//String sql;
		//sql = "select * from correlation_tbl where correlation_tbl_ID < 5";
		//sql= "select * from correlation_tbl where gene_set_A in ( \"Pentose and glucuronate interconversions (KEGG)\", \"Caffeine metabolism (KEGG)\"  , \"{VCP,17} (Static Module)\" ,  \"{HSPA8,34} (Static Module)\" ) AND gene_set_B in ( \"Pentose and glucuronate interconversions (KEGG)\", \"Caffeine metabolism (KEGG)\"  , \"{VCP,17} (Static Module)\" ,  \"{HSPA8,34} (Static Module)\" )";
		ResultSet rs = stmt.executeQuery(sql2);

		//STEP 5: Extract data from result set
		while(rs.next()){
			//Retrieve by column name
			//correlation_tbl_ID INT NOT NULL AUTO_INCREMENT,
			//edge TEXT NOT NULL,
			//gene_set_A VARCHAR(200) NOT NULL,
			//gene_set_B VARCHAR(200) NOT NULL,
			//partial_correlation float NOT NULL,
			//p_value double NOT NULL,
			//overlap_coefficient float NOT NULL,
			//p_adjust double NOT NULL,
			//int correlation_tbl_ID  = rs.getInt("correlation_tbl_ID");
			//String edge = rs.getString("edge");
			String gene_set_A = rs.getString("gene_set_A");
			String gene_set_B = rs.getString("gene_set_B");
			float partial_correlation = rs.getFloat("partial_correlation");
			double p_value = rs.getDouble("p_value");
			float overlap_coefficient = rs.getFloat("overlap_coefficient");
			//double p_adjust = rs.getDouble("p_adjust");

			//store values
			JSONArray list = new JSONArray();
			//list.add(correlation_tbl_ID);
			//list.add(edge);
			list.add(gene_set_A);
			list.add(gene_set_B);
			list.add(partial_correlation);
			list.add(p_value);
			list.add(overlap_coefficient);
			//list.add(p_adjust);
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