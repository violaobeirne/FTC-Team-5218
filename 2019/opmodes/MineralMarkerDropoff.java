package opmodes;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/27/2018.
 */
public class MineralMarkerDropoff {
    public enum DropMarker {
        TRUE,
        FALSE,
        DEFAULT;
    }

    public enum GoldMineralPosition {
        LEFT,
        RIGHT,
        CENTER,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][3];

    public MineralMarkerDropoff() {
        // true paths tested on Monday
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 50, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 18, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 110, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35, 0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 32, 0.3);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 23, 0.5);



        // robot will move backwards with positive power
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 60, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 160, 0.4);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 50, 0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 18, -0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 100, 0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.5);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 38, -0.3);
        paths[MineralMarkerDropoff.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35, 0.5);

        // false paths not finalized or tested
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 50, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 18, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 55, 0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.4);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.4);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30, 0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -1.0);

        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 60, -1.0);

        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 50, 0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 18, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 55, -0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.4);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30, 0.5);
        paths[MineralMarkerDropoff.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -1.0);
    }

    public DeadReckonPath getPath(MineralMarkerDropoff.DropMarker markerDrop, MineralMarkerDropoff.GoldMineralPosition goldPosition)
    {
        return paths[markerDrop.ordinal()][goldPosition.ordinal()];
    }
}
