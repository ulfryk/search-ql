/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, OrderedSet } from 'immutable';

import { Match } from './match';
import { MatchCoords } from './match-coords';

const text = ('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor' +
  ' incididunt ut labore et dolore magna aliqua.').toLowerCase();

const getCoords = (phrase: string) => MatchCoords.fromIndex(phrase)(text.indexOf(phrase));

const getMatch = (phrase: string) =>
  Match.fromIndexes(text, phrase, OrderedSet([text.indexOf(phrase)]));

describe('Match', () => {

  describe('equals method', () => {

    it('should properly test for equality', () => {
      text
        .split(' ')
        .map(phrase => [phrase, phrase].map(getMatch))
        .forEach(([lhs, rhs]) => {
          expect(lhs.equals(rhs)).to.be.true;
        });
      text
        .split(' ')
        .map(phrase => [phrase, 'consectetur adipiscing elit'].map(getMatch))
        .forEach(([lhs, rhs]) => {
          expect(lhs.equals(rhs)).to.be.false;
        });
    });

    it('should be reflexive', () => {
      text.split(' ').map(getMatch).forEach(match => {
        expect(match.equals(match)).to.be.true;
      });
    });

    it('should be symmetric', () => {
      text
        .split(' ')
        .map(phrase => [phrase, phrase].map(getMatch))
        .concat(text
          .split(', ')
          .map(phrase => [phrase, 'et, consectetur adipiscing elit, se'].map(getMatch)))
        .forEach(([lhs, rhs]) => {
          expect(lhs.equals(rhs)).to.be.equal(rhs.equals(lhs));
        });
    });

    it('should be transitive', () => {
      text
        .split(' ')
        .map(phrase => [phrase, phrase, phrase].map(getMatch))
        .concat(text
          .split(', ')
          .map(phrase => [phrase, 'et, conse', 'et, conse'].map(getMatch)))
        .concat(text
          .split(', ')
          .map(phrase => ['et, conse', phrase, 'et, conse'].map(getMatch)))
        .forEach(([a, b, c]) => {
          const lhs = a.equals(b) && b.equals(c);
          expect(lhs).to.equal(a.equals(c) && lhs);
        });
    });

    it('should be consistent', () => {
      text
        .split(', ')
        .map(phrase => [phrase, phrase].map(getMatch))
        .forEach(([lhs, rhs]) => {
          Array(50).fill(lhs).map(oneOf => oneOf.equals(rhs)).forEach(result => {
            expect(result).to.be.true;
          });
        });
      text
        .split(', ')
        .map(phrase => [phrase, 'scing elit, se'].map(getMatch))
        .forEach(([lhs, rhs]) => {
          Array(50).fill(lhs).map(oneOf => oneOf.equals(rhs)).forEach(result => {
            expect(result).to.be.false;
          });
        });
    });

  });

  describe('and method', () => {

    it('should merge Matches', () => {
      const rhs = getMatch('ipsum').and(getMatch('dolor sit')).and(getMatch('dolore'));
      const lhs = new Match(text, Map([
        ['dolor sit', OrderedSet([getCoords('dolor sit')])],
        ['dolore', OrderedSet([getCoords('dolore')])],
        ['ipsum', OrderedSet([getCoords('ipsum')])],
      ]));

      expect(lhs.equals(rhs)).to.be.true;
    });

    it('should merge Matches and drop duplicates', () => {
      const rhs = getMatch('ipsum').and(getMatch('ipsum')).and(getMatch('ipsum'));
      const lhs = new Match(text, Map([
        ['ipsum', OrderedSet([getCoords('ipsum')])],
      ]));

      expect(lhs.equals(rhs)).to.be.true;
    });

    it('should be associative', () => {
      const lhs = getMatch('ipsum')
        .and(getMatch('dolor sit'))
        .and(getMatch('dolore').and(getMatch('ipsum')));
      const rhs = getMatch('ipsum')
        .and(getMatch('dolor sit').and(getMatch('dolore')))
        .and(getMatch('ipsum'));

      expect(lhs.equals(rhs)).to.be.true;
    });

    it('should be commutative', () => {
      const lhs = getMatch('ipsum')
        .and(getMatch('dolor sit'))
        .and(getMatch('dolore'));
      const rhs = getMatch('dolor sit')
        .and(getMatch('ipsum'))
        .and(getMatch('dolore'));

      expect(lhs.equals(rhs)).to.be.true;
    });

  });

  describe('getFlatMatched method', () => {

    it('should properly merge intersecting coords in output', () => {
      const match1 = getMatch('dolor').and(getMatch('dolor sit')).and(getMatch('sit amet'));
      const match2 = getMatch(' ut labore').and(getMatch(' et dolore'));
      const match3 = getMatch('elit, sed do');
      const lhs = match1.and(match2).and(match3).getFlatMatched();
      const rhs = OrderedSet([
        getCoords('dolor sit amet'),
        getCoords('elit, sed do'),
        getCoords(' ut labore et dolore'),
      ]);

      expect(lhs).to.deep.equal(rhs);
    });

  });

});
