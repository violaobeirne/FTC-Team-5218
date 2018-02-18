package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.navigation.VuforiaLocalizer;

import team25core.ColorThiefTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;
import team25core.TwoAxisShoulderTask;
import team25core.VuMarkIdentificationTask;
import team25core.VuforiaBase;

/**
 * Created by Lizzie on 11/11/2017.
 */

@Autonomous (name = "5218 Autonomous")
public class BeethovenAutonomous extends Robot {
    protected enum AllianceColor {
        BLUE,
        RED,
        DEFAULT,
    }

    protected AllianceColor allianceColor;
    protected GlyphDeposits.StartingPosition startPosition;

    private static boolean detectedRed = false;

    public static final double GLYPH_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_OPEN_LEFT_POSITION;
    public static final double GLYPH_CLOSE_LEFT_POSITION = HisaishiCalibration.GLYPH_CLOSE_LEFT_POSITION;
    public static final double GLYPH_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_OPEN_RIGHT_POSITION;
    public static final double GLYPH_CLOSE_RIGHT_POSITION = HisaishiCalibration.GLYPH_CLOSE_RIGHT_POSITION;

    public static final int BLUE_BLUE_LOWER = HisaishiCalibration.BLUE_BLUE_LOWER;
    public static final int BLUE_RED_UPPER = HisaishiCalibration.BLUE_RED_UPPER;
    public static final int RED_RED_LOWER = HisaishiCalibration.RED_RED_LOWER;
    public static final int RED_BLUE_UPPER = HisaishiCalibration.RED_BLUE_UPPER;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo glyphLGrabber;
    private Servo glyphRGrabber;
    private Servo jewelXServo;
    private Servo jewelYServo;
    private DcMotor glyphElevator;

    private GamepadTask gamepad;

    private DeadReckonPath glyphPath;
    private GlyphDeposits glyphDeposit;
    private DeadReckonPath pushGlyph;

    private FourWheelDirectDrivetrain drivetrain;
    private ColorThiefTask colorThiefTask;
    private VuforiaBase vuforiaBase;
    private VuMarkIdentificationTask vumkTask;
    private TwoAxisShoulderTask shoulderTask;

    private Telemetry.Item particleColor;
    private Telemetry.Item alliance;
    private Telemetry.Item startingPosition;
    private Telemetry.Item column;

    private boolean pollingOn = false;
    private boolean flashOn = false;

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamepadSelection(event);
        }
        RobotLog.i("Vumark: Detected " + e.toString());
    }

    @Override
    public void init() {
        vuforiaBase = new VuforiaBase();
        vuforiaBase.setCameraDirection(VuforiaLocalizer.CameraDirection.FRONT);
        vuforiaBase.init(this);

        allianceColor = AllianceColor.DEFAULT;
        startPosition = GlyphDeposits.StartingPosition.DEFAULT;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        startingPosition = telemetry.addData("Starting Position: ", "NOT SELECTED");
        particleColor = telemetry.addData("Jewel Color:", "BLACK");
        column = telemetry.addData("Column: ", "NOT SELECTED");

        frontLeft = hardwareMap.dcMotor.get("frontL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backLeft = hardwareMap.dcMotor.get("backL");
        backRight = hardwareMap.dcMotor.get("backR");
        glyphElevator = hardwareMap.dcMotor.get("glyphElevator");

        glyphRGrabber = hardwareMap.servo.get("glyphRightGrabber");
        glyphLGrabber = hardwareMap.servo.get("glyphLeftGrabber");

        jewelXServo = hardwareMap.servo.get("jewelXAxis");
        jewelYServo = hardwareMap.servo.get("jewelYAxis");


        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setSplitPersonalityMotorDirection(false);

        glyphElevator.setMode(DcMotor.RunMode.RESET_ENCODERS);
        glyphElevator.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        glyphDeposit = new GlyphDeposits();
        pushGlyph = new DeadReckonPath();

        detectParticle();

        vumarkIdentification();
    }


    @Override
    public void start() {
        RobotLog.i("104 Closing glyph arms and raising glyph.");
        servoPosClose();
        liftGlyph();

        addTask(new SingleShotTimerTask(this, 1000) {
            @Override
            public void handleEvent(RobotEvent e) {
                RobotLog.i("104 Glyph retrieved, preparing to push particle.");
                pushParticle();
            }
        });
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
        switch (event.kind) {
            case BUTTON_X_DOWN:
                allianceColor = AllianceColor.BLUE;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = AllianceColor.RED;
                alliance.setValue("RED");
                break;
            case BUTTON_A_DOWN:
                if (pollingOn) {
                    colorThiefTask.setPollingMode(ColorThiefTask.PollingMode.OFF);
                    vumkTask.setPollingMode(VuMarkIdentificationTask.PollingMode.OFF);
                    pollingOn = false;
                } else if (!pollingOn) {
                    colorThiefTask.setPollingMode(ColorThiefTask.PollingMode.ON);
                    vumkTask.setPollingMode(VuMarkIdentificationTask.PollingMode.ON);
                    pollingOn = true;
                }
                break;
            case BUTTON_Y_DOWN:
                com.vuforia.CameraDevice.getInstance().setFlashTorchMode(!flashOn);
                flashOn = !flashOn;
                break;
            case LEFT_BUMPER_DOWN:
                startPosition = GlyphDeposits.StartingPosition.R1;
                startingPosition.setValue("R1");
                break;
            case LEFT_TRIGGER_DOWN:
                startPosition = GlyphDeposits.StartingPosition.R2;
                startingPosition.setValue("R2");
                break;
            case RIGHT_BUMPER_DOWN:
                startPosition = GlyphDeposits.StartingPosition.B1;
                startingPosition.setValue("B1");
                break;
            case RIGHT_TRIGGER_DOWN:
                startPosition = GlyphDeposits.StartingPosition.B2;
                startingPosition.setValue("B2");
                break;
        }
    }

    protected void vumarkIdentification() {
        vumkTask = new VuMarkIdentificationTask(this, vuforiaBase) {
            @Override
            public void handleEvent(RobotEvent e) {
                VuMarkIdentificationEvent event = (VuMarkIdentificationEvent) e;
                switch (event.kind) {
                    case LEFT:
                        glyphPath = glyphDeposit.getPath(startPosition, GlyphDeposits.TargetColumn.LEFT);
                        column.setValue("LEFT");
                        break;
                    case CENTER:
                        glyphPath = glyphDeposit.getPath(startPosition, GlyphDeposits.TargetColumn.MIDDLE);
                        column.setValue("MIDDLE");
                        break;
                    case RIGHT:
                        glyphPath = glyphDeposit.getPath(startPosition, GlyphDeposits.TargetColumn.RIGHT);
                        column.setValue("RIGHT");
                        break;
                    default:
                        break;
                }
            }
        };
        this.addTask(vumkTask);
    }

    protected void liftGlyph() {
        // use actual encoder thing
        // will eliminate ghost behavior
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                glyphElevator.setPower(0.6);
                addTask(new SingleShotTimerTask(this.robot, 700) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        glyphElevator.setPower(0);
                    }
                });
            }
        });
    }

    protected void detectParticle() {
        colorThiefTask = new ColorThiefTask(this, vuforiaBase) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorThiefTask.ColorThiefEvent event = (ColorThiefEvent) e;
                switch (event.kind) {
                    case RED:
                        particleColor.setValue("RED");
                        detectedRed = true;
                        RobotLog.i("104 Detected red");
                        break;
                    case BLUE:
                        particleColor.setValue("BLUE");
                        detectedRed = false;
                        RobotLog.i("104 Detected blue");
                        break;
                    case BLACK:
                        particleColor.setValue("BLACK");
                        RobotLog.i("104 Detected black");
                        break;
                }
            }
        };
        colorThiefTask.setThresholds(RED_RED_LOWER, RED_BLUE_UPPER, BLUE_BLUE_LOWER, BLUE_RED_UPPER);
        this.addTask(colorThiefTask);

        shoulderTask = new TwoAxisShoulderTask(this, jewelXServo, jewelYServo) {
            @Override
            public void handleEvent(RobotEvent e) {
                initialMove(glyphPath);
            }

        };
        shoulderTask.init();
    }

    protected void pushParticle() {
        RobotLog.i("104 knocking opponent's jewel off the platform.");
        if (startPosition == startPosition.R1 || startPosition == startPosition.R2) {
            if (detectedRed) {
                shoulderTask.setDirection(TwoAxisShoulderTask.ShoulderDirection.BACKWARD);
            } else if (!detectedRed) {
                shoulderTask.setDirection(TwoAxisShoulderTask.ShoulderDirection.FORWARD);
            }
        } else if (startPosition == startPosition.B1 || startPosition == startPosition.B2) {
            if (!detectedRed) {
                shoulderTask.setDirection(TwoAxisShoulderTask.ShoulderDirection.BACKWARD);
            } else if (detectedRed) {
                shoulderTask.setDirection(TwoAxisShoulderTask.ShoulderDirection.FORWARD);
            }
        }
        this.addTask(shoulderTask);
    }

    protected void pushGlyph() {
        pushGlyph.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 2, -0.3);
        pushGlyph.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.3);
        pushGlyph.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3, -0.3);
        pushGlyph.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 4, 0.3);
        pushGlyph.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.3);

        this.addTask(new DeadReckonTask(this, pushGlyph, drivetrain));
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("104 opening glyph arms after path to key column finish.");
                        servoPosOpen();
                        addTask(new SingleShotTimerTask(this.robot, 500) {
                            @Override
                            public void handleEvent(RobotEvent e) {
                                RobotLog.i("104 securing glyph into key column with two pushes.");
                                pushGlyph();
                            }
                        });
                }
            }
        });
    }

    protected void servoPosOpen() {
        glyphLGrabber.setPosition(GLYPH_OPEN_LEFT_POSITION);
        glyphRGrabber.setPosition(GLYPH_OPEN_RIGHT_POSITION);
    }

    protected void servoPosClose() {
        glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
        glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
    }
}
