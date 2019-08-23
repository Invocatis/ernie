
public class DatabaseCase {

  @Autowired
  private Database db;

// --- Dynamic Type Implementation

  @Action("insert")
  public Object insert(String table, Map<String, Object> params) {

    Map<String, Object> entry = db.insert(table, params);
    id = entry.get("id");
    return entry;
  }

  @Verify("insert")
  public boolean verifyInsert(String table, Map<String, Object> params, Object result) {

    Object result = db.read(table, result.getId());

    return (result != null);
  }

  @Clean("insert")
  public void cleanInsert(String table, Map<String, Object> params, Object result) {

    db.deleteById(table, result.getId());
  }

// --- Static Type Implementation ---

  @Action("insert-project")
  public ProjectDTO insertProject(Map<String, Object> params) {

    return db.insert(ProjectDTO.class, params);
  }

  public boolean verifyInsertProject(Map<String, Object> params, ProjectDTO result) {

    final ProjectDTO fromDatabase = db.readById(ProjectDTO.class, result.getId);

    return fromDatabase != null
        && params.get("name") == fromDatabase.getName()
        && params.get("principle-investigator") == fromDatabase;
  }

  public void cleanInsertProject(Map<String, Object> params, ProjectDTO result) {

    db.delete(ProjectDTO.class, result);
  }
}
