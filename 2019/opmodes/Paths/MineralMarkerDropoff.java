package opmodes.Paths;

import opmodes.Utilities.MineralUtils;
import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/27/2018.
 */
public class MineralMarkerDropoff {

    DeadReckonPath[][] paths = new DeadReckonPath[2][3];

    public MineralMarkerDropoff() {
        // true paths tested on Monday
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 28, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 29, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 70, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 12, 0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);
        /*
        // old routes
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 20, 0.5);
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 29, -0.7);
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 100, -0.5);
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 12, 0.5);
        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 23, 0.5);

        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, -1.0);
        */

        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 20, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 29, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 100, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 12, 0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 23, 0.4);

        // paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, -1.0);

        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 15, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 2, 0.5);
        /*
        // old paths
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 60, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 140, 0.4);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        */

        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 60, -1.0);

        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 28, 0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 29, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 115, 0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 2, 0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 12, -0.5);
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 0.5);

        // paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, -1.0);

        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 28, 0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 29, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 115, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.4);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 12, -0.5);
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 23, 0.4);
        // paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, -0.4);
    }

    public DeadReckonPath getPath(MineralUtils.DropMarker markerDrop, MineralUtils.MineralPosition goldPosition)
    {
        return paths[markerDrop.ordinal()][goldPosition.ordinal()];
    }
}
