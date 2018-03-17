import { expect } from 'chai';
import { Set } from 'immutable';

import { MatchCoords, Ordering } from './match-coords';

const text = 'lorem ipsum dolor';
const getCoords = (phrase: string) => MatchCoords.fromIndex(phrase)(text.indexOf(phrase));

describe('MatchCoords', () => {

  describe('compare method', () => {

    it('should return proper Ordering value', () => {
      expect(getCoords('lor').compare(getCoords('lor'))).to.equal(Ordering.Eq);
      expect(getCoords('rem ipsu').compare(getCoords('rem ipsu'))).to.equal(Ordering.Eq);
      expect(getCoords('sum').compare(getCoords('sum'))).to.equal(Ordering.Eq);

      expect(getCoords('lorem').compare(getCoords('orem'))).to.equal(Ordering.Lt);
      expect(getCoords('lorem').compare(getCoords('lorem ip'))).to.equal(Ordering.Lt);

      expect(getCoords('orem').compare(getCoords('lorem'))).to.equal(Ordering.Gt);
      expect(getCoords('lorem').compare(getCoords('lor'))).to.equal(Ordering.Gt);
    });

    it('should allow sorting collections of MatchCoords', () => {
      const matched = [
        getCoords('orem'),
        getCoords('rem ipsu'),
        getCoords('orem'),
        getCoords('ipsum'),
        getCoords('lorem'),
        getCoords('rem'),
        getCoords('rem ip'),
        getCoords('lorem'),
      ];

      expect(Set(matched).sort((a, b) => a.compare(b)).toString())
        .to.equal('OrderedSet { [0, 5], [1, 5], [2, 5], [2, 8], [2, 10], [6, 11] }');
    });

  });

});
