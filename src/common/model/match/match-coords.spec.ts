/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Set } from 'immutable';

import { Ordering } from '../ord';
import { MatchCoords } from './match-coords';

const text = ('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor' +
  ' incididunt ut labore et dolore magna aliqua.').toLowerCase();

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

  describe('intersects method', () => {

    const intersecting = [
      ['ipsum dolor', 'dolor sit'],
      ['elit,', ', sed do'],
      ['et dol', 'ore magna'],
    ].map(phrases => phrases.map(getCoords));

    const notIntersecting = [
      ['ipsum dolor', 'sit amet'],
      ['sit amet', 'adipiscing elit'],
      ['rem ipsu', 'a aliqua.'],
    ].map(phrases => phrases.map(getCoords));

    it('should properly recognize intersecting coordinates', () => {
      intersecting.forEach(([a, b]) => {
        expect(a.intersects(b)).be.true;
      });
    });

    it('should fail test for NOT intersecting coordinates', () => {
      notIntersecting.forEach(([a, b]) => {
        expect(a.intersects(b)).to.be.false;
      });
    });

  });

  describe('merging coords', () => {

    const mergeable = [
      ['ipsum dolor', 'dolor sit', 'ipsum dolor sit'],
      ['elit,', ', sed do', 'elit, sed do'],
      ['et dol', 'ore magna', 'et dolore magna'],
    ].map(phrases => phrases.map(getCoords));

    const notMergeable = [
      ['ipsum dolor', 'sit amet'],
      ['sit amet', 'adipiscing elit'],
      ['rem ipsu', 'a aliqua.'],
    ].map(phrases => phrases.map(getCoords));

    describe('merge method', () => {

      it('should properly merge subsequent, intersecting coordinates', () => {
        mergeable.forEach(([a, b, c]) => {
          expect(a.merge(b)).to.deep.equal(c);
        });
      });

    });

    describe('queueOrMerge method', () => {

      it('should properly merge subsequent, intersecting coordinates', () => {
        mergeable.forEach(([a, b, c]) => {
          expect(a.queueOrMerge(b)).to.deep.equal([c]);
        });
      });

      it('should NOT merge NOT intersecting coordinates', () => {
        notMergeable.forEach(([a, b]) => {
          expect(a.queueOrMerge(b)).to.deep.equal([a, b]);
        });
      });

    });

  });

});
