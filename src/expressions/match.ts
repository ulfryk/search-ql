import { Map, Set } from 'immutable';

import { MatchCoords } from './match-coords';

export class Match {

  constructor(
    public readonly input: string,
    public readonly matched: Map<string, Set<MatchCoords>>,
  ) {}

  public toString() {
    return `Match "${this.input}" { ${this.matched} }`;
  }

}
