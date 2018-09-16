export class MatchError extends Error {

  public static improperConjunction(inputA: string, inputB: string) {
    return new MatchError(`Match: 'and' operator should NOT be used on different text matches:` +
      ` "${inputA}" !== "${inputB}"`);
  }

  public static improperQueueOrMerge(coordsA: string, coordsB: string) {
    return new MatchError(`MatchCoords: coords passed to 'queueOrMerge' method (${coordsB})` +
      ` should be higher than self: ${coordsA}`);
  }

  public static improperMerge(coordsA: string, coordsB: string) {
    return new MatchError(`MatchCoords: coords passed to 'merge' method (${coordsB}) ` +
      ` should intersect and be higher than self: ${coordsA}`);
  }

  public readonly name = 'MatchError';

}
