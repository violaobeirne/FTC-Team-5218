package opmodes.Paths;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 10/26/2018.
 */
public class MarkerDropoff {
    public enum StartingPosition {
        CLOSE,
        FAR,
        DEFAULT;
    }

    public enum DepotPath {
        LEFT,
        RIGHT,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[3][3];

    public MarkerDropoff() {
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 18, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 70, -0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 60, 0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, 0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 15, 0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5);

        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 18, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 50, 0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 80, -0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 14, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 20, -0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5);

        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 80, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 30, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, 0.5);

        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 13, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 70, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 15, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 22, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, 0.5);
    }

    public DeadReckonPath getPath(MarkerDropoff.StartingPosition landing, MarkerDropoff.DepotPath path)
    {
        return paths[landing.ordinal()][path.ordinal()];
    }


}
