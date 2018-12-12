package opmodes;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/27/2018.
 */
public class MarkerDropoff2 {
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

    public MarkerDropoff2() {
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45, -0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 25, -0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30, 0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35, -0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 180, 0.3);

        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 60, 0.5);

        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45, 0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 25, -0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30, -0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, -0.5);
        paths[MarkerDropoff2.DropMarker.TRUE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 180, 0.5);


        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 45, 0.4);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 80, 0.4);

        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.4);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5);

        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 160, 0.4);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 25, 0.5);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 70, -0.4);
        paths[MarkerDropoff2.DropMarker.FALSE.ordinal()][GoldMineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.5);
    }

    public DeadReckonPath getPath(MarkerDropoff2.DropMarker markerDrop, MarkerDropoff2.GoldMineralPosition goldPosition)
    {
        return paths[markerDrop.ordinal()][goldPosition.ordinal()];
    }
}
