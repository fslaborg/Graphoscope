namespace Graphoscope


/// <summary> 
/// Edge representation with generic node key and edge data seperated by default
/// </summary>
type Edge<'NodeKey, 'EdgeData> = 'NodeKey * 'NodeKey * 'EdgeData 